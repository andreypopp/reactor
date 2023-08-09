open! Import

module Computation : sig
  type t

  val root :
    (t * Html.t list ref * int -> Html.t Lwt.t) ->
    (Html.t * Html.t Lwt_stream.t option) Lwt.t

  val fork :
    t ->
    (t -> [ `Fail of exn | `Fork of Html.t Lwt.t * 'a | `Sync of 'a ]) ->
    'a Lwt.t

  val use_idx : t -> int
  val emit_import : t -> Html.t -> unit
end = struct
  type ctx = {
    mutable idx : int;
    mutable pending : int;
    push : Html.t option -> unit;
  }

  type t = {
    ctx : ctx;
    emit_import : Html.t -> unit;
    finished : unit Lwt.t;
  }

  let use_idx t =
    t.ctx.idx <- t.ctx.idx + 1;
    t.ctx.idx

  let emit_import t html = t.emit_import html

  let root f =
    let rendering, push = Lwt_stream.create () in
    let idx = 0 in
    let ctx = { push; pending = 1; idx } in
    let imports = ref [] in
    let finished, parent_done = Lwt.wait () in
    let emit_import import = imports := import :: !imports in
    f ({ ctx; emit_import; finished }, imports, idx) >|= fun html ->
    Lwt.wakeup_later parent_done ();
    ctx.pending <- ctx.pending - 1;
    match ctx.pending = 0 with
    | true ->
        ctx.push None;
        html, None
    | false -> html, Some rendering

  let fork parent_t f =
    let ctx = parent_t.ctx in
    let finished, parent_done = Lwt.wait () in
    let emit_import html = ctx.push (Some html) in
    let t = { ctx; emit_import; finished } in
    match f t with
    | `Fork (async, sync) ->
        ctx.pending <- ctx.pending + 1;
        Lwt.async (fun () ->
            parent_t.finished >>= fun () ->
            async >|= fun html ->
            ctx.push (Some html);
            Lwt.wakeup_later parent_done ();
            ctx.pending <- ctx.pending - 1;
            if ctx.pending = 0 then ctx.push None);
        Lwt.return sync
    | `Sync sync ->
        Lwt.wakeup_later parent_done ();
        Lwt.return sync
    | `Fail exn -> Lwt.fail exn
end

module Emit_html = struct
  let html_rc =
    Html.unsafe_rawf "<script>%s</script>"
      {|$RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};|}

  let html_chunk idx html =
    Html.(
      splice ~sep:"\n"
        [
          node "div"
            [ "hidden", b true; "id", s (sprintf "S:%i" idx) ]
            [ html ];
          unsafe_rawf "<script>$RC('B:%i', 'S:%i')</script>" idx idx;
        ])

  let html_suspense inner =
    Html.(
      splice [ unsafe_rawf "<!--$?-->"; inner; unsafe_rawf "<!--/$-->" ])

  let html_suspense_placeholder idx =
    html_suspense
    @@ Html.unsafe_rawf {|<template id="B:%i"></template>|} idx

  let splice = Html.splice ~sep:"<!-- -->"
end

module Emit_model = struct
  let html_start =
    Html.unsafe_rawf
      {|<script>
          let enc = new TextEncoder();
          let React_of_caml_ssr = (window.React_of_caml_ssr = {});
          React_of_caml_ssr.push = () => {
            let el = document.currentScript;
            React_of_caml_ssr._c.enqueue(enc.encode(el.dataset.payload))
          };
          React_of_caml_ssr.close = () => {
            React_of_caml_ssr._c.close();
          };
          React_of_caml_ssr.stream = new ReadableStream({ start(c) { React_of_caml_ssr._c = c; } });
        </script> |}

  let html_model model =
    let chunk = Render_to_model.chunk_to_string model in
    Html.unsafe_rawf
      "<script data-payload='%s'>window.React_of_caml_ssr.push()</script>"
      (Html.single_quote_escape chunk)

  let html_end =
    Html.unsafe_rawf "<script>window.React_of_caml_ssr.close()</script>"
end

let rec client_to_html t = function
  | React.El_null -> Lwt.return (Html.text "")
  | El_text s -> Lwt.return (Html.text s)
  | El_html (name, props, None) -> Lwt.return (Html.node name props [])
  | El_html (name, props, Some (Html_children children)) ->
      client_to_html_many t children >|= fun children ->
      Html.node name props [ children ]
  | El_html (name, props, Some (Html_children_raw { __html })) ->
      Lwt.return (Html.node name props [ Html.unsafe_raw __html ])
  | El_thunk f ->
      let rec wait () =
        match f () with
        | exception React.Suspend (Any_promise promise) ->
            promise >>= fun _ -> wait ()
        | v -> client_to_html t v
      in
      wait ()
  | El_async_thunk _ -> failwith "async component in client mode"
  | El_suspense { children; fallback = _ } -> (
      match Array.length children with
      | 0 -> Lwt.return (Emit_html.html_suspense Html.empty)
      | _ ->
          Computation.fork t @@ fun t ->
          let idx = Computation.use_idx t in
          let async =
            client_to_html_many t children >|= Emit_html.html_chunk idx
          in
          `Fork (async, Emit_html.html_suspense_placeholder idx))
  | El_client_thunk
      { import_module = _; import_name = _; props = _; thunk } ->
      client_to_html t thunk

and client_to_html_many t els : Html.t Lwt.t =
  Array.to_list els
  |> Lwt_list.map_p (client_to_html t)
  >|= Emit_html.splice

let rec server_to_html t = function
  | React.El_null -> Lwt.return (Html.empty, Render_to_model.null)
  | El_text s -> Lwt.return (Html.text s, Render_to_model.text s)
  | El_html (name, props, Some (Html_children children)) ->
      server_to_html_many t children >|= fun (html, children) ->
      ( Html.node name props [ html ],
        Render_to_model.node ~name
          ~props:(props :> (string * json) list)
          (Some children) )
  | El_html (name, props, Some (Html_children_raw { __html })) ->
      Lwt.return
        ( Html.node name props [ Html.unsafe_raw __html ],
          let props = (props :> (string * json) list) in
          let props =
            ("dangerouslySetInnerHTML", `Assoc [ "__html", `String __html ])
            :: props
          in
          Render_to_model.node ~name ~props None )
  | El_html (name, props, None) ->
      Lwt.return
        ( Html.node name props [],
          Render_to_model.node ~name
            ~props:(props :> (string * json) list)
            None )
  | El_thunk f -> server_to_html t (f ())
  | El_async_thunk f -> f () >>= server_to_html t
  | El_suspense { children; fallback = _ } -> (
      Computation.fork t @@ fun t ->
      let promise = server_to_html_many t children in
      match Lwt.state promise with
      | Sleep ->
          let idx = Computation.use_idx t in
          let html_async =
            promise >|= fun (html, model) ->
            Html.splice
              [
                Emit_model.html_model (idx, Render_to_model.C_tree model);
                Emit_html.html_chunk idx html;
              ]
          in
          let html_sync =
            ( Emit_html.html_suspense_placeholder idx,
              Render_to_model.suspense_placeholder idx )
          in
          `Fork (html_async, html_sync)
      | Return (html, model) ->
          `Sync
            (Emit_html.html_suspense html, Render_to_model.suspense model)
      | Fail exn -> `Fail exn)
  | El_client_thunk { import_module; import_name; props; thunk } ->
      let props =
        Lwt_list.map_p
          (fun (name, jsony) ->
            match jsony with
            | `Element element ->
                server_to_html t element >|= fun (_html, model) ->
                name, model
            | #json as jsony -> Lwt.return (name, (jsony :> json)))
          props
      in
      let html = client_to_html t thunk in
      Lwt.both html props >|= fun (html, props) ->
      let model =
        let idx = Computation.use_idx t in
        let ref = Render_to_model.ref ~import_module ~import_name in
        Computation.emit_import t (Emit_model.html_model (idx, C_ref ref));
        Render_to_model.node ~name:(sprintf "$%i" idx) ~props None
      in
      html, model

and server_to_html_many finished els =
  Array.to_list els
  |> Lwt_list.map_p (server_to_html finished)
  >|= List.split
  >|= fun (htmls, model) ->
  Emit_html.splice htmls, Render_to_model.list model

type html_rendering =
  | Html_rendering_done of { html : Html.t }
  | Html_rendering_async of {
      html_shell : Html.t;
      html_iter : (Html.t -> unit Lwt.t) -> unit Lwt.t;
    }

let render el =
  let html =
    Computation.root @@ fun (t, imports, idx) ->
    server_to_html t el >|= fun (html, model) ->
    Html.splice
      (!imports
      @ [
          Emit_model.html_model (idx, Render_to_model.C_tree model); html;
        ])
  in
  html >|= fun (html_shell, async) ->
  match async with
  | None ->
      let html =
        Html.splice
          [ html_shell; Emit_model.html_start; Emit_model.html_end ]
      in
      Html_rendering_done { html }
  | Some async ->
      let html_iter f =
        Lwt_stream.iter_s f async >>= fun () -> f Emit_model.html_end
      in
      let html_shell =
        Html.(
          splice [ Emit_model.html_start; Emit_html.html_rc; html_shell ])
      in
      Html_rendering_async { html_shell; html_iter }
