open! Import

type instruction =
  | I_html of Html.t  (** output HTML *)
  | I_htmli of int * Html.t  (** output incremental HTML *)
  | I_model of (int * Render_to_model.chunk)  (** output RSC model *)

type ctx = {
  mutable idx : int;
  mutable waiting : int;
  push : instruction option -> unit;
}

let push_html ctx html = ctx.push (Some (I_html html))
let push_model ctx model = ctx.push (Some (I_model model))
let push_htmli ctx idx html = ctx.push (Some (I_htmli (idx, html)))

let use_idx ctx =
  ctx.idx <- ctx.idx + 1;
  ctx.idx

let html_suspense inner =
  Html.(
    splice [ unsafe_rawf "<!--$?-->"; inner; unsafe_rawf "<!--/$-->" ])

let html_suspense_placeholder idx =
  html_suspense
  @@ Html.unsafe_rawf {|<template id="B:%i"></template>|} idx

let html_sep = "<!-- -->"

let client_to_html ctx parent el =
  let rec client_to_html' parent = function
    | React.El_null -> Lwt.return (Html.text "")
    | El_text s -> Lwt.return (Html.text s)
    | El_html (name, props, None) ->
        Lwt.return (Html.node name props None)
    | El_html (name, props, Some children) ->
        client_to_html_many' parent children >|= fun children ->
        Html.node name props (Some [ children ])
    | El_thunk f ->
        let rec wait () =
          match f () with
          | exception React.Suspend (Any_promise promise) ->
              promise >>= fun _ -> wait ()
          | v -> client_to_html' parent v
        in
        wait ()
    | El_async_thunk _ -> failwith "async component in client mode"
    | El_suspense { children; fallback = _ } -> (
        match Array.length children with
        | 0 -> Lwt.return (html_suspense Html.empty)
        | _ ->
            let parent', ready = Lwt.wait () in
            let idx = use_idx ctx in
            ctx.waiting <- ctx.waiting + 1;
            Lwt.async (fun () ->
                client_to_html_many' parent' children >>= fun html ->
                parent >|= fun () ->
                push_htmli ctx idx html;
                Lwt.wakeup_later ready ();
                ctx.waiting <- ctx.waiting - 1;
                if ctx.waiting = 0 then ctx.push None);
            Lwt.return (html_suspense_placeholder idx))
    | El_client_thunk
        { import_module = _; import_name = _; props = _; thunk } ->
        client_to_html' parent thunk
  and client_to_html_many' parent els : Html.t Lwt.t =
    Array.to_list els
    |> Lwt_list.map_p (client_to_html' parent)
    >|= Html.splice ~sep:html_sep
  in
  client_to_html' parent el

let to_html ctx el =
  let rec server_to_html' parent = function
    | React.El_null -> Lwt.return (Html.text "", Render_to_model.null)
    | El_text s -> Lwt.return (Html.text s, Render_to_model.text s)
    | El_html (name, props, Some children) ->
        server_to_html_many' parent children >|= fun (html, children) ->
        ( Html.node name props (Some [ html ]),
          Render_to_model.node ~name
            ~props:(props :> (string * json) list)
            (Some children) )
    | El_html (name, props, None) ->
        Lwt.return
          ( Html.node name props None,
            Render_to_model.node ~name
              ~props:(props :> (string * json) list)
              None )
    | El_thunk f -> server_to_html' parent (f ())
    | El_async_thunk f -> f () >>= server_to_html' parent
    | El_suspense { children; fallback = _ } -> (
        let parent', ready = Lwt.wait () in
        let promise = server_to_html_many' parent' children in
        match Lwt.state promise with
        | Sleep ->
            let idx = use_idx ctx in
            ctx.waiting <- ctx.waiting + 1;
            Lwt.async (fun () ->
                promise >>= fun (html, model) ->
                parent >|= fun () ->
                push_htmli ctx idx html;
                push_model ctx (idx, Render_to_model.C_tree model);
                Lwt.wakeup_later ready ();
                ctx.waiting <- ctx.waiting - 1;
                if ctx.waiting = 0 then ctx.push None);
            Lwt.return
              ( html_suspense_placeholder idx,
                Render_to_model.suspense_placeholder idx )
        | Return (html, model) ->
            Lwt.wakeup_later ready ();
            Lwt.return (html_suspense html, Render_to_model.suspense model)
        | Fail exn -> Lwt.fail exn)
    | El_client_thunk { import_module; import_name; props; thunk } ->
        let props =
          Lwt_list.map_p
            (fun (name, jsony) ->
              match jsony with
              | `Element element ->
                  server_to_html' parent element >|= fun (_html, model) ->
                  name, model
              | #json as jsony -> Lwt.return (name, (jsony :> json)))
            props
        in
        let html = client_to_html ctx parent thunk in
        Lwt.both html props >|= fun (html, props) ->
        let model =
          let idx = use_idx ctx in
          let ref = Render_to_model.ref ~import_module ~import_name in
          push_model ctx (idx, C_ref ref);
          Render_to_model.node ~name:(sprintf "$%i" idx) ~props None
        in
        html, model
  and server_to_html_many' parent els =
    Array.to_list els
    |> Lwt_list.map_p (server_to_html' parent)
    >|= List.split
    >|= fun (htmls, model) ->
    Html.splice htmls ~sep:html_sep, Render_to_model.list model
  in
  let idx = ctx.idx in
  let parent, ready = Lwt.wait () in
  server_to_html' parent el >|= fun (html, json) ->
  push_html ctx html;
  push_model ctx (idx, Render_to_model.C_tree json);
  Lwt.wakeup_later ready ();
  ctx.waiting <- ctx.waiting - 1;
  if ctx.waiting = 0 then ctx.push None

(* RENDERING *)

let rc =
  Html.unsafe_rawf "<script>%s</script>"
    {|$RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};|}
  |> Html.to_string

let render_js fmt =
  ksprintf
    (fun js -> Html.(node "script" [] (Some [ unsafe_rawf "{%s}" js ])))
    fmt

let render_ssr_runtime =
  render_js "%s"
    {|
let enc = new TextEncoder();
let SSR = (window.SSR = {});
SSR.push = (data) => SSR._c.enqueue(enc.encode(data));
SSR.close = () => SSR._c.close();
SSR.stream = new ReadableStream({ start(c) { SSR._c = c; } });|}
  |> Html.to_string

let render_ssr_finish = render_js "window.SSR.close()" |> Html.to_string

let render_html_chunk idx html =
  Html.splice ~sep:"\n"
    [
      Html.node "div"
        [ "hidden", `Bool true; "id", `String (sprintf "S:%i" idx) ]
        (Some [ html ]);
      Html.unsafe_rawf {|<script>$RC("B:%i", "S:%i")</script>|} idx idx;
    ]
  |> Html.to_string

let render ?on_shell_ready el on_chunk =
  let rendering, push = Lwt_stream.create () in
  let ctx = { push; waiting = 1; idx = 0 } in
  to_html ctx el >>= fun () ->
  let render_rc =
    let sent = ref false in
    fun () ->
      match !sent with
      | true -> Lwt.return ()
      | false ->
          sent := true;
          on_chunk rc
  in
  on_chunk render_ssr_runtime >>= fun () ->
  let render = function
    | I_model chunk ->
        let chunk = Render_to_model.chunk_to_string chunk in
        on_chunk
          Html.(
            render_js "window.SSR.push(%S)"
              ((* not the most effective... consider patching yojson instead? *)
               json_escape chunk)
            |> to_string)
    | I_html html -> (
        on_chunk (Html.to_string html) >>= fun () ->
        match on_shell_ready with None -> Lwt.return () | Some f -> f ())
    | I_htmli (idx, html) ->
        render_rc () >>= fun () -> on_chunk (render_html_chunk idx html)
  in
  Lwt_stream.iter_s render rendering >>= fun () ->
  on_chunk render_ssr_finish
