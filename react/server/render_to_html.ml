open! Import

module Computation : sig
  type t

  val root :
    (t * int -> Html.t Lwt.t) ->
    (Html.t * Html.t Lwt_stream.t option) Lwt.t

  val fork :
    t ->
    (t -> [ `Fail of exn | `Fork of Html.t Lwt.t * 'a | `Sync of 'a ]) ->
    'a Lwt.t

  val update_ctx : t -> 'a React_model.context -> 'a -> t
  val with_ctx : t -> (unit -> 'a) -> 'a * Remote.Runner.running list

  val with_ctx_async :
    t -> (unit -> 'a Lwt.t) -> ('a * Remote.Runner.running list) Lwt.t

  val use_idx : t -> int
  val emit_html : t -> Html.t -> unit
end = struct
  type ctx = {
    mutable idx : int;
    mutable pending : int;
    push : Html.t option -> unit;
    remote_runner_ctx : Remote.Runner.ctx;
  }

  type t = {
    ctx : ctx;
    react_ctx : Hmap.t;
    finished : unit Lwt.t;
    mutable emit_html : Html.t -> unit;
  }

  let update_ctx t ctx v =
    let react_ctx = Hmap.add ctx.React_model.key v t.react_ctx in
    { t with react_ctx }

  let with_ctx t f =
    let f () = React_model.with_context t.react_ctx f in
    Remote.Runner.with_ctx t.ctx.remote_runner_ctx f

  let with_ctx_async t f =
    let f () = React_model.with_context t.react_ctx f in
    Remote.Runner.with_ctx_async t.ctx.remote_runner_ctx f

  let use_idx t =
    t.ctx.idx <- t.ctx.idx + 1;
    t.ctx.idx

  let emit_html t html = t.emit_html html

  let root f =
    let rendering, push = Lwt_stream.create () in
    let idx = 0 in
    let ctx =
      {
        push;
        pending = 1;
        idx;
        remote_runner_ctx = Remote.Runner.create ();
      }
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
    let html = Html.splice [ Html.splice htmls; html ] in
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
          React_of_caml_ssr.push_rpc = () => {
            let el = document.currentScript;
            let path = el.dataset.path;
            let input = el.dataset.input;
            let output = el.dataset.output;
            window.__Remote_cache = window.__Remote_cache || {};
            window.__Remote_cache[path] = window.__Remote_cache[path] || {};
            window.__Remote_cache[path][input] = {json: Promise.resolve(output), data: null};
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

  let html_rpc_payload path input output =
    Html.unsafe_rawf
      "<script data-path='%s' data-input='%s' \
       data-output='%s'>window.React_of_caml_ssr.push_rpc()</script>"
      (Html.single_quote_escape path)
      (Html.single_quote_escape (Yojson.Basic.to_string input))
      (Html.single_quote_escape (Yojson.Basic.to_string output))

  let html_end =
    Html.unsafe_rawf "<script>window.React_of_caml_ssr.close()</script>"
end

let handle_remote_reqs t reqs =
  Lwt_list.map_p
    (fun (Remote.Runner.Running
           { path; input; promise; yojson_of_output }) ->
      promise >|= fun output ->
      let html =
        Emit_model.html_rpc_payload path input (yojson_of_output output)
      in
      html)
    reqs
  >|= fun payload -> Computation.emit_html t (Html.splice payload)

let rec client_to_html t = function
  | React_model.El_null -> Lwt.return (Html.text "")
  | El_text s -> Lwt.return (Html.text s)
  | El_frag els -> client_to_html_many t els
  | El_context (key, value, children) ->
      let t = Computation.update_ctx t key value in
      client_to_html t children
  | El_html { tag_name; key = _; props; children = None } ->
      Lwt.return (Html.node tag_name props [])
  | El_html
      {
        tag_name;
        key = _;
        props;
        children = Some (Html_children children);
      } ->
      client_to_html t children >|= fun children ->
      Html.node tag_name props [ children ]
  | El_html
      {
        tag_name;
        key = _;
        props;
        children = Some (Html_children_raw { __html });
      } ->
      Lwt.return (Html.node tag_name props [ Html.unsafe_raw __html ])
  | El_thunk f ->
      let rec wait () =
        match Computation.with_ctx t f with
        | exception React_model.Suspend (Any_promise promise) ->
            promise >>= fun _ -> wait ()
        | v, [] -> client_to_html t v
        | v, reqs ->
            let payload = handle_remote_reqs t reqs in
            let children = client_to_html t v in
            Lwt.both children payload >|= fun (children, ()) -> children
      in
      wait ()
  | El_async_thunk _ -> failwith "async component in client mode"
  | El_suspense { children; fallback = _; key = _ } -> (
      match children with
      | El_null -> Lwt.return (Emit_html.html_suspense Html.empty)
      | _ ->
          Computation.fork t @@ fun t ->
          let idx = Computation.use_idx t in
          let async =
            client_to_html t children >|= Emit_html.html_chunk idx
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
  | React_model.El_null -> Lwt.return (Html.empty, Render_to_model.null)
  | El_text s -> Lwt.return (Html.text s, Render_to_model.text s)
  | El_frag els -> server_to_html_many t els
  | El_context _ ->
      failwith "react context is not supported in server environment"
  | El_html
      { tag_name; key; props; children = Some (Html_children children) }
    ->
      server_to_html t children >|= fun (html, children) ->
      ( Html.node tag_name props [ html ],
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
        ( Html.node tag_name props [ Html.unsafe_raw __html ],
          let props = (props :> (string * json) list) in
          let props =
            ( "dangerouslySetInnerHTML",
              `Assoc [ "__html", `String __html ] )
            :: props
          in
          Render_to_model.node ~tag_name ~key ~props None )
  | El_html { tag_name; key; props; children = None } ->
      Lwt.return
        ( Html.node tag_name props [],
          Render_to_model.node ~tag_name ~key
            ~props:(props :> (string * json) list)
            None )
  | El_thunk f ->
      let tree, _reqs = Computation.with_ctx t f in
      (* TODO: need to register them somewhere? *)
      (* let payload = handle_remote_reqs t reqs in *)
      server_to_html t tree
  | El_async_thunk f ->
      Computation.with_ctx_async t f >>= fun (tree, _reqs) ->
      server_to_html t tree
  | El_suspense { children; fallback = _; key } -> (
      Computation.fork t @@ fun t ->
      let promise = server_to_html t children in
      match Lwt.state promise with
      | Sleep ->
          let idx = Computation.use_idx t in
          let html_async =
            promise >|= fun (html, model) ->
            Html.splice
              [
                Emit_model.html_model (idx, Render_to_model.C_value model);
                Emit_html.html_chunk idx html;
              ]
          in
          let html_sync =
            ( Emit_html.html_suspense_placeholder idx,
              Render_to_model.suspense_placeholder ~key idx )
          in
          `Fork (html_async, html_sync)
      | Return (html, model) ->
          `Sync
            ( Emit_html.html_suspense html,
              Render_to_model.suspense ~key model )
      | Fail exn -> `Fail exn)
  | El_client_thunk { import_module; import_name; props; thunk } ->
      let props =
        Lwt_list.map_p
          (fun (name, jsony) ->
            match jsony with
            | React_model.Element element ->
                server_to_html t element >|= fun (_html, model) ->
                name, model
            | Promise (promise, value_to_yojson) ->
                Computation.fork t @@ fun t ->
                let idx = Computation.use_idx t in
                let sync = name, Render_to_model.promise_value idx in
                let async =
                  promise >|= fun value ->
                  let json = value_to_yojson value in
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
        let idx = Computation.use_idx t in
        let ref = Render_to_model.ref ~import_module ~import_name in
        Computation.emit_html t (Emit_model.html_model (idx, C_ref ref));
        Render_to_model.node ~tag_name:(sprintf "$%i" idx) ~key:None
          ~props None
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
    Computation.root @@ fun (t, idx) ->
    server_to_html t el >|= fun (html, model) ->
    Html.splice
      [ Emit_model.html_model (idx, Render_to_model.C_value model); html ]
  in
  html >|= fun (html_shell, async) ->
  match async with
  | None ->
      let html =
        Html.splice
          [ Emit_model.html_start; html_shell; Emit_model.html_end ]
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
