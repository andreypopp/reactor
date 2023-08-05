open! Import

type instruction =
  | I_wait of instruction Lwt.t
      (** wait for instruction to be available and execute it, this is needed
            so we only output incremental HTML once the placeholder for is ready *)
  | I_html of Html.t  (** output HTML *)
  | I_htmli of int * Html.t  (** output incremental HTML *)
  | I_model of (int * Render_to_model.chunk)  (** output RSC model *)

type ctx = {
  mutable idx : int;
  mutable waiting : int;
  push : instruction option -> unit;
}

let push_wait ctx =
  let p, u = Lwt.wait () in
  ctx.push (Some (I_wait p));
  u

let push_model ctx model = ctx.push (Some (I_model model))
let push_html_chunk ctx idx html = ctx.push (Some (I_htmli (idx, html)))

let use_idx ctx =
  ctx.idx <- ctx.idx + 1;
  ctx.idx

let html_placeholder idx =
  Html.rawf {|<!--$?--><template id="B:%i"></template><!--/$-->|} idx

let to_html ctx el =
  let rec client_to_html' : React.element -> Html.t Lwt.t = function
    | React.El_null -> Lwt.return (Html.text "")
    | El_text s -> Lwt.return (Html.text s)
    | El_html (name, props, children) ->
        let children =
          match children with
          | None -> Lwt.return_none
          | Some children -> client_to_html_many' children >|= Option.some
        in
        children >|= fun children ->
        Html.node name props (Option.map (fun h -> [ h ]) children)
    | El_thunk f -> client_to_html' (f ())
    | El_async_thunk _ -> failwith "async component in client mode"
    | El_suspense { children; fallback = _ } -> (
        match Array.length children with
        | 0 -> Lwt.return (Html.raw "<!--$?--><!--/$-->")
        | _ ->
            let restart () = client_to_html_many' children in
            let rec wait (React.Any_promise promise) =
              promise >>= fun _ ->
              Lwt.catch restart (function
                | React.Suspend any_promise -> wait any_promise
                | exn -> Lwt.fail exn)
            in
            Lwt.catch restart (function
              | React.Suspend any_promise ->
                  let idx = use_idx ctx in
                  ctx.waiting <- ctx.waiting + 1;
                  Lwt.async (fun () ->
                      wait any_promise >|= fun html ->
                      push_html_chunk ctx idx html;
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push None);
                  Lwt.return (html_placeholder idx)
              | exn -> Lwt.fail exn))
    | El_client_thunk
        { import_module = _; import_name = _; props = _; thunk } ->
        client_to_html' thunk
  and client_to_html_many' els : Html.t Lwt.t =
    Array.to_list els
    |> Lwt_list.map_p client_to_html'
    >|= Html.splice ~sep:"<!-- -->"
  and server_to_html' = function
    | React.El_null -> Lwt.return (Html.text "", Render_to_model.null)
    | El_text s -> Lwt.return (Html.text s, Render_to_model.text s)
    | El_html (name, props, children) ->
        (match children with
        | None -> Lwt.return_none
        | Some children -> server_to_html_many' children >|= Option.some)
        >|= fun children ->
        let html, children =
          match children with
          | None -> None, None
          | Some (html, children) -> Some [ html ], Some children
        in
        ( Html.node name props html,
          Render_to_model.node ~name
            ~props:(props :> (string * json) list)
            children )
    | El_thunk f -> server_to_html' (f ())
    | El_async_thunk f -> f () >>= server_to_html'
    | El_suspense { children; fallback = _ } -> (
        let promise = server_to_html_many' children in
        match Lwt.state promise with
        | Sleep ->
            let idx = use_idx ctx in
            ctx.waiting <- ctx.waiting + 1;
            Lwt.async (fun () ->
                promise >|= fun (html, model) ->
                push_html_chunk ctx idx html;
                push_model ctx (idx, Render_to_model.C_tree model);
                ctx.waiting <- ctx.waiting - 1;
                if ctx.waiting = 0 then ctx.push None);
            Lwt.return
              ( html_placeholder idx,
                Render_to_model.suspense_placeholder idx )
        | Return (html, model) ->
            Lwt.return (html, Render_to_model.suspense model)
        | Fail exn -> Lwt.fail exn)
    | El_client_thunk { import_module; import_name; props; thunk } ->
        let props =
          Lwt_list.map_p
            (fun (name, jsony) ->
              match jsony with
              | `Element element ->
                  server_to_html' element >|= fun (_html, model) ->
                  name, model
              | #json as jsony -> Lwt.return (name, (jsony :> json)))
            props
        in
        let html = client_to_html' thunk in
        Lwt.both props html >|= fun (props, html) ->
        let model =
          let idx = use_idx ctx in
          let ref = Render_to_model.ref ~import_module ~import_name in
          push_model ctx (idx, C_ref ref);
          Render_to_model.node ~name:(sprintf "$%i" idx) ~props None
        in
        html, model
  and server_to_html_many' els =
    Array.to_list els |> Lwt_list.map_p server_to_html' >|= List.split
    >|= fun (htmls, model) ->
    Html.splice htmls ~sep:"<!-- -->", Render_to_model.list model
  in
  let idx = ctx.idx in
  let ready = push_wait ctx in
  server_to_html' el >|= fun (html, json) ->
  push_model ctx (idx, Render_to_model.C_tree json);
  Lwt.wakeup ready (I_html html);
  ctx.waiting <- ctx.waiting - 1;
  if ctx.waiting = 0 then ctx.push None

(* RENDERING *)

let rc =
  Html.rawf "<script>%s</script>"
    {|$RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};|}
  |> Html.to_string

let render_js fmt =
  ksprintf
    (fun js ->
      Html.(node "script" [] (Some [ rawf "{%s}" js ]) |> to_string))
    fmt

let render_ssr_runtime =
  render_js "%s"
    {|
let enc = new TextEncoder();
let SSR = (window.SSR = {});
SSR.push = (data) => SSR._c.enqueue(enc.encode(data));
SSR.close = () => SSR._c.close();
SSR.stream = new ReadableStream({ start(c) { SSR._c = c; } });|}

let render_html_chunk idx html =
  Html.splice ~sep:"\n"
    [
      Html.node "div"
        [ "hidden", `Bool true; "id", `String (sprintf "S:%i" idx) ]
        (Some [ html ]);
      Html.rawf {|<script>$RC("B:%i", "S:%i")</script>|} idx idx;
    ]
  |> Html.to_string

let render el on_chunk =
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
  let rec render = function
    | I_wait ps -> ps >>= render
    | I_model chunk ->
        let chunk = Render_to_model.chunk_to_string chunk in
        on_chunk (render_js "window.SSR.push(%S)" chunk)
    | I_html html -> on_chunk (Html.to_string html)
    | I_htmli (idx, html) ->
        render_rc () >>= fun () -> on_chunk (render_html_chunk idx html)
  in
  Lwt_stream.iter_s render rendering >>= fun () ->
  on_chunk (render_js "window.SSR.close()")
