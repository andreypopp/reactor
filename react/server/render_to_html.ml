open! Import

module Computation : sig
  type t

  val root :
    (t * int -> Htmlgen.t Lwt.t) ->
    (Htmlgen.t * Htmlgen.t Lwt_stream.t option) Lwt.t

  val fork :
    t ->
    (t -> [ `Fail of exn | `Fork of Htmlgen.t Lwt.t * 'a | `Sync of 'a ]) ->
    'a Lwt.t

  val update_ctx : t -> 'a React_model.context -> 'a -> t
  val with_ctx : t -> (unit -> 'a) -> 'a * Remote.Context.batch

  val with_ctx_async :
    t -> (unit -> 'a Lwt.t) -> ('a * Remote.Context.batch) Lwt.t

  val use_idx : t -> int
  val emit_html : t -> Htmlgen.t -> unit
  val emit_batch : t -> Remote.Context.batch -> unit Lwt.t
end = struct
  type ctx = {
    mutable idx : int;
    mutable pending : int;
    push : Htmlgen.t option -> unit;
    remote_ctx : Remote.Context.t;
  }

  type t = {
    ctx : ctx;
    react_ctx : Hmap.t;
    finished : unit Lwt.t;
    mutable emit_html : Htmlgen.t -> unit;
  }

  let update_ctx t ctx v =
    let react_ctx = Hmap.add ctx.React_model.key v t.react_ctx in
    { t with react_ctx }

  let with_ctx t f =
    let f () = React_model.with_context t.react_ctx f in
    Remote.Context.with_ctx t.ctx.remote_ctx f

  let with_ctx_async t f =
    let f () = React_model.with_context t.react_ctx f in
    Remote.Context.with_ctx_async t.ctx.remote_ctx f

  let use_idx t =
    t.ctx.idx <- t.ctx.idx + 1;
    t.ctx.idx

  let emit_html t html = t.emit_html html

  let emit_batch t batch =
    Remote.Context.batch_to_html t.ctx.remote_ctx batch >|= fun html ->
    emit_html t html

  let root f =
    let rendering, push = Lwt_stream.create () in
    let idx = 0 in
    let ctx =
      { push; pending = 1; idx; remote_ctx = Remote.Context.create () }
    in
    let htmls = ref (Some []) in
    let finished, parent_done = Lwt.wait () in
    let emit_html chunk =
      match !htmls with
      | Some chunks -> htmls := Some (chunk :: chunks)
      | None -> failwith "invariant violation: root computation finished"
    in
    f ({ ctx; emit_html; finished; react_ctx = Hmap.empty }, idx)
    >|= fun html ->
    let htmls =
      match !htmls with
      | Some chunks ->
          htmls := None;
          chunks
      | None -> assert false
    in
    let html = Htmlgen.splice [ Htmlgen.splice htmls; html ] in
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
    let t =
      {
        ctx;
        emit_html = parent_t.emit_html;
        react_ctx = parent_t.react_ctx;
        finished;
      }
    in
    match f t with
    | `Fork (async, sync) ->
        ctx.pending <- ctx.pending + 1;
        t.emit_html <- (fun html -> ctx.push (Some html));
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
    Htmlgen.unsafe_rawf "<script>%s</script>"
      {|$RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};|}

  let html_chunk idx html =
    Htmlgen.(
      splice ~sep:"\n"
        [
          node "div"
            [ "hidden", b true; "id", s (sprintf "S:%x" idx) ]
            [ html ];
          unsafe_rawf "<script>$RC('B:%x', 'S:%x')</script>" idx idx;
        ])

  let html_suspense inner =
    Htmlgen.(
      splice [ unsafe_rawf "<!--$?-->"; inner; unsafe_rawf "<!--/$-->" ])

  let html_suspense_placeholder (fallback : Htmlgen.t) idx =
    Htmlgen.(
      html_suspense
        (splice
           [
             unsafe_rawf {|<template id="B:%x"></template>|} idx; fallback;
           ]))

  let splice = Htmlgen.splice ~sep:"<!-- -->"
end

module Emit_model = struct
  let html_start =
    Htmlgen.unsafe_rawf
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
    Htmlgen.unsafe_rawf
      "<script data-payload='%s'>window.React_of_caml_ssr.push()</script>"
      (Htmlgen.single_quote_escape chunk)

  let html_end =
    Htmlgen.unsafe_rawf
      "<script>window.React_of_caml_ssr.close()</script>"
end

let rec client_to_html t = function
  | React_model.El_null -> Lwt.return (Htmlgen.text "")
  | El_text s -> Lwt.return (Htmlgen.text s)
  | El_frag els -> client_to_html_many t els
  | El_context (key, value, children) ->
      let t = Computation.update_ctx t key value in
      client_to_html t children
  | El_html { tag_name; key = _; props; children = None } ->
      Lwt.return (Htmlgen.node tag_name props [])
  | El_html
      {
        tag_name;
        key = _;
        props;
        children = Some (Html_children children);
      } ->
      client_to_html t children >|= fun children ->
      Htmlgen.node tag_name props [ children ]
  | El_html
      {
        tag_name;
        key = _;
        props;
        children = Some (Html_children_raw { __html });
      } ->
      Lwt.return
        (Htmlgen.node tag_name props [ Htmlgen.unsafe_raw __html ])
  | El_thunk f ->
      let rec wait () =
        match Computation.with_ctx t f with
        | exception React_model.Suspend (Any_promise promise) ->
            promise >>= fun _ -> wait ()
        | v, batch ->
            Lwt.both (client_to_html t v) (Computation.emit_batch t batch)
            >|= fst
      in
      wait ()
  | El_async_thunk _ -> failwith "async component in client mode"
  | El_suspense { children; fallback; key = _ } -> (
      match children with
      | El_null -> Lwt.return (Emit_html.html_suspense Htmlgen.empty)
      | _ ->
          client_to_html t fallback >>= fun fallback ->
          Computation.fork t @@ fun t ->
          let idx = Computation.use_idx t in
          let async =
            client_to_html t children >|= Emit_html.html_chunk idx
          in
          `Fork (async, Emit_html.html_suspense_placeholder fallback idx))
  | El_client_thunk
      { import_module = _; import_name = _; props = _; thunk } ->
      client_to_html t thunk

and client_to_html_many t els : Htmlgen.t Lwt.t =
  Array.to_list els
  |> Lwt_list.map_p (client_to_html t)
  >|= Emit_html.splice

let rec server_to_html ~render_model t = function
  | React_model.El_null -> Lwt.return (Htmlgen.empty, Render_to_model.null)
  | El_text s ->
      Lwt.return
        ( Htmlgen.text s,
          if not render_model then Render_to_model.null
          else Render_to_model.text s )
  | El_frag els -> server_to_html_many ~render_model t els
  | El_context _ ->
      failwith "react context is not supported in server environment"
  | El_html
      { tag_name; key; props; children = Some (Html_children children) }
    ->
      server_to_html ~render_model t children >|= fun (html, children) ->
      ( Htmlgen.node tag_name props [ html ],
        if not render_model then Render_to_model.null
        else
          Render_to_model.node ~tag_name ~key
            ~props:(props :> (string * json) list)
            (Some children) )
  | El_html
      {
        tag_name;
        key;
        props;
        children = Some (Html_children_raw { __html });
      } ->
      Lwt.return
        ( Htmlgen.node tag_name props [ Htmlgen.unsafe_raw __html ],
          if not render_model then Render_to_model.null
          else
            let props = (props :> (string * json) list) in
            let props =
              ( "dangerouslySetInnerHTML",
                `Assoc [ "__html", `String __html ] )
              :: props
            in
            Render_to_model.node ~tag_name ~key ~props None )
  | El_html { tag_name; key; props; children = None } ->
      Lwt.return
        ( Htmlgen.node tag_name props [],
          if not render_model then Render_to_model.null
          else
            Render_to_model.node ~tag_name ~key
              ~props:(props :> (string * json) list)
              None )
  | El_thunk f ->
      let tree, _reqs = Computation.with_ctx t f in
      (* NOTE: ignoring [_reqs] here as server data requests won't be replayed
         on client, while we still want to allow to use Remote library *)
      server_to_html ~render_model t tree
  | El_async_thunk f ->
      Computation.with_ctx_async t f >>= fun (tree, _reqs) ->
      (* NOTE: ignoring [_reqs] here as server data requests won't be replayed
         on client, while we still want to allow to use Remote library *)
      server_to_html ~render_model t tree
  | El_suspense { children; fallback; key } -> (
      server_to_html ~render_model t fallback
      >>= fun (fallback, fallback_model) ->
      Computation.fork t @@ fun t ->
      let promise = server_to_html ~render_model t children in
      match Lwt.state promise with
      | Sleep ->
          let idx = Computation.use_idx t in
          let html_async =
            promise >|= fun (html, model) ->
            let html = [ Emit_html.html_chunk idx html ] in
            let html =
              if not render_model then html
              else
                Emit_model.html_model (idx, Render_to_model.C_value model)
                :: html
            in
            Htmlgen.splice html
          in
          let html_sync =
            ( Emit_html.html_suspense_placeholder fallback idx,
              if not render_model then Render_to_model.null
              else
                Render_to_model.suspense_placeholder ~key
                  ~fallback:fallback_model idx )
          in
          `Fork (html_async, html_sync)
      | Return (html, model) ->
          `Sync
            ( Emit_html.html_suspense html,
              if not render_model then Render_to_model.null
              else
                Render_to_model.suspense ~key ~fallback:fallback_model
                  model )
      | Fail exn -> `Fail exn)
  | El_client_thunk { import_module; import_name; props; thunk } ->
      let props =
        Lwt_list.map_p
          (fun (name, jsony) ->
            match jsony with
            | React_model.Element element ->
                server_to_html ~render_model t element
                >|= fun (_html, model) -> name, model
            | Promise (promise, value_to_json) ->
                Computation.fork t @@ fun t ->
                let idx = Computation.use_idx t in
                let sync =
                  ( name,
                    if not render_model then Render_to_model.null
                    else Render_to_model.promise_value idx )
                in
                let async =
                  promise >|= fun value ->
                  let json = value_to_json value in
                  Emit_model.html_model (idx, C_value json)
                in
                `Fork (async, sync)
            | Json json -> Lwt.return (name, json))
          props
      in
      let html = client_to_html t thunk in
      (* NOTE: this Lwt.pause () is important as we resolve client component in
         an async way we need to suspend above, otherwise React.js runtime won't work *)
      Lwt.pause () >>= fun () ->
      Lwt.both html props >|= fun (html, props) ->
      let model =
        if not render_model then Render_to_model.null
        else
          let idx = Computation.use_idx t in
          let ref = Render_to_model.ref ~import_module ~import_name in
          Computation.emit_html t (Emit_model.html_model (idx, C_ref ref));
          Render_to_model.node ~tag_name:(sprintf "$%x" idx) ~key:None
            ~props None
      in
      html, model

and server_to_html_many ~render_model finished els =
  Array.to_list els
  |> Lwt_list.map_p (server_to_html ~render_model finished)
  >|= List.split
  >|= fun (htmls, model) ->
  Emit_html.splice htmls, Render_to_model.list model

type html_rendering =
  | Html_rendering_done of { html : Htmlgen.t }
  | Html_rendering_async of {
      html_shell : Htmlgen.t;
      html_iter : (Htmlgen.t -> unit Lwt.t) -> unit Lwt.t;
    }

let render ~render_model el =
  let html =
    Computation.root @@ fun (t, idx) ->
    server_to_html ~render_model t el >|= fun (html, model) ->
    if not render_model then html
    else
      Htmlgen.splice
        [
          Emit_model.html_model (idx, Render_to_model.C_value model); html;
        ]
  in
  html >|= fun (html_shell, async) ->
  match async with
  | None ->
      let html =
        if not render_model then html_shell
        else
          Htmlgen.splice
            [ Emit_model.html_start; html_shell; Emit_model.html_end ]
      in
      Html_rendering_done { html }
  | Some async ->
      let html_iter f =
        Lwt_stream.iter_s f async >>= fun () ->
        if not render_model then Lwt.return () else f Emit_model.html_end
      in
      let html_shell =
        if not render_model then
          Htmlgen.splice [ Emit_html.html_rc; html_shell ]
        else
          Htmlgen.(
            splice
              [ Emit_model.html_start; Emit_html.html_rc; html_shell ])
      in
      Html_rendering_async { html_shell; html_iter }
