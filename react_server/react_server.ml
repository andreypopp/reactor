open ContainersLabels
open Monomorphic
open Printf
open Lwt.Infix

type json = Yojson.Safe.t
(* used to transfer props to/from browser *)

type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise
(* React.use raises this when the promise is not resolved yet *)

module React = struct
  type element =
    | El_null : element
    | El_suspense : {
        children : children;
        fallback : children;
      }
        -> element
    | El_text : string -> element
    | El_html : string * html_props * children option -> element
    | El_thunk : (unit -> element) -> element
    | El_async_thunk : (unit -> element Lwt.t) -> element
    | El_client_thunk : {
        import_module : string;
        import_name : string;
        props : client_props;
        thunk : element;
      }
        -> element

  and children = element array
  and html_props = (string * json) list
  and client_props = (string * [ json | `Element of element ]) list

  let null = El_null
  let text s = El_text s
  let textf fmt = ksprintf text fmt
  let thunk f = El_thunk f
  let async_thunk f = El_async_thunk f

  let suspense ?(fallback = [| null |]) children =
    El_suspense { children; fallback }

  let client_thunk ?(import_name = "") import_module props thunk =
    El_client_thunk { import_module; import_name; props; thunk }

  type html_element = ?className:string -> children -> element

  let html' tag_name : html_element =
   fun ?className children ->
    let props =
      let string v = `String v in
      [ "className", Option.map string className ]
      |> List.filter_map ~f:(function
           | _, None -> None
           | n, Some v -> Some (n, v))
    in
    El_html (tag_name, props, Some children)

  let h = html'
  let html = html' "html"
  let body = html' "body"
  let head = html' "head"
  let title = html' "title"
  let div = html' "div"
  let a = html' "a"
  let span = html' "span"
  let li = html' "li"
  let ul = html' "ul"
  let ol = html' "ol"
  let h1 = html' "h1"
  let h2 = html' "h2"
  let h3 = html' "h3"
  let use_effect _thunk _deps = assert false
  let use_effect' _thunk _deps = assert false

  type 'a promise = 'a Lwt.t

  let use _promise = assert false
end

module React_browser = struct
  module Promise = struct
    type 'a promise = 'a Lwt.t

    let sleep secs = Lwt_unix.sleep secs
  end

  module React = struct
    include React

    let use_effect _thunk _deps = ()
    let use_effect' _thunk _deps = ()

    type 'a promise = 'a Lwt.t

    let use promise =
      match Lwt.state promise with
      | Return v -> v
      | Sleep -> raise (Suspend (Any_promise promise))
      | Fail exn -> raise exn
  end
end

module Render_to_model : sig
  val render : React.element -> (Buffer.t -> unit Lwt.t) -> unit Lwt.t
end = struct
  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    push : Buffer.t option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  let push_ref ctx idx ref =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (sprintf "%x:I" idx);
    Yojson.Safe.write_json buf ref;
    Buffer.add_char buf '\n';
    ctx.push (Some buf)

  let push ctx idx json =
    let buf = Buffer.create (4 * 1024) in
    Buffer.add_string buf (sprintf "%x:" idx);
    Yojson.Safe.write_json buf json;
    Buffer.add_char buf '\n';
    ctx.push (Some buf)

  let rec to_model ctx idx el =
    let rec to_model' = function
      | React.El_null -> `Null
      | El_text s -> `String s
      | El_html (tag_name, props, None) ->
          `List [ `String "$"; `String tag_name; `Null; `Assoc props ]
      | El_html (tag_name, props, Some children) ->
          `List
            [
              `String "$";
              `String tag_name;
              `Null;
              `Assoc
                (( "children",
                   `List (Array.to_list children |> List.map ~f:to_model')
                 )
                :: props);
            ]
      | El_suspense { children; fallback = _ } ->
          `List
            [
              `String "$";
              `String "$Sreact.suspense";
              `Null;
              `Assoc
                [
                  ( "children",
                    `List (Array.to_list children |> List.map ~f:to_model')
                  );
                ];
            ]
      | El_thunk f -> to_model' (f ())
      | El_async_thunk f -> (
          let tree = f () in
          match Lwt.state tree with
          | Lwt.Return tree -> to_model' tree
          | Lwt.Fail exn -> raise exn
          | Lwt.Sleep ->
              let idx = use_idx ctx in
              ctx.waiting <- ctx.waiting + 1;
              Lwt.async (fun () -> Lwt.map (to_model ctx idx) tree);
              `String (sprintf "$L%i" idx))
      | El_client_thunk { import_module; import_name; props; thunk = _ }
        ->
          let idx = use_idx ctx in
          let ref =
            `Assoc
              [
                "id", `String import_module;
                "name", `String import_name;
                "chunks", `List [];
                "async", `Bool false;
              ]
          in
          push_ref ctx idx ref;
          let props = List.map props ~f:json_model_to_model in
          `List
            [
              `String "$";
              `String (sprintf "$%i" idx);
              `Null;
              `Assoc props;
            ]
    and json_model_to_model (name, jsony) =
      match jsony with
      | `Element element -> name, to_model' element
      | #json as jsony -> name, (jsony :> json)
    in
    push ctx idx (to_model' el);
    if ctx.waiting = 0 then ctx.push None

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx = { push; waiting = 0; idx = 0 } in
    to_model ctx ctx.idx el;
    Lwt_stream.iter_s on_chunk rendering
end

module Render_to_html = struct
  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    push : (string * (int * string)) option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  let emit_html tag_name props children =
    let attrs =
      List.map props ~f:(fun (name, value) ->
          let name =
            match name with "className" -> "class" | name -> name
          in
          sprintf "%s=%s" name (Yojson.Safe.to_string value))
      |> String.concat ~sep:" "
    in
    match children with
    | None -> sprintf {|<%s %s/>|} tag_name attrs
    | Some children ->
        sprintf {|<%s %s>%s</%s>|} tag_name attrs children tag_name

  type mode = Mode_server | Mode_client

  let instruction_RC =
    {|
    $RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};
    |}

  let to_html ctx mode el =
    let rec to_html this mode = function
      | React.El_null -> Lwt.return ("", `Null)
      | El_text s -> Lwt.return (s, `String s)
      | El_html (tag_name, props, None) ->
          Lwt.return
            ( emit_html tag_name props None,
              `List [ `String "$"; `String tag_name; `Null; `Assoc props ]
            )
      | El_html (tag_name, props, Some children) ->
          to_html_many this mode children >|= fun (html, model) ->
          ( emit_html tag_name props (Some html),
            `List
              [
                `String "$";
                `String tag_name;
                `Null;
                `Assoc (("children", model) :: props);
              ] )
      | El_suspense { children; fallback = _ } -> (
          match mode with
          | Mode_client -> (
              match Array.length children with
              | 0 ->
                  Lwt.return
                    ( {|<!--$?--><!--/$-->|},
                      `List
                        [
                          `String "$";
                          `String "$Sreact.suspense";
                          `Null;
                          `Assoc [];
                        ] )
              | _ ->
                  let restart () = to_html_many this mode children in
                  let rec wait (Any_promise promise) =
                    promise >>= fun _ ->
                    Lwt.catch restart (function
                      | Suspend any_promise -> wait any_promise
                      | exn -> Lwt.fail exn)
                    >>= fun v ->
                    this >|= fun () -> v
                  in
                  Lwt.catch restart (function
                    | Suspend any_promise ->
                        let idx = use_idx ctx in
                        ctx.waiting <- ctx.waiting + 1;
                        Lwt.async (fun () ->
                            wait any_promise >|= fun (html, json) ->
                            let html =
                              sprintf {|<div hidden id="S:%i">%s</div>|}
                                idx html
                            in
                            let hydrate =
                              sprintf
                                {|<script>%s$RC("B:%i", "S:%i")</script>|}
                                instruction_RC idx idx
                            in
                            ctx.push
                              (Some
                                 ( html ^ hydrate,
                                   (idx, Yojson.Safe.to_string json) ));
                            ctx.waiting <- ctx.waiting - 1;
                            if ctx.waiting = 0 then ctx.push None);
                        Lwt.return
                          ( sprintf
                              {|<!--$?--><template id="B:%i"></template><!--/$-->|}
                              idx,
                            `List
                              [
                                `String "$";
                                `String "$Sreact.suspense";
                                `Null;
                                `Assoc
                                  [
                                    ( "children",
                                      `String (sprintf "$L%i" idx) );
                                  ];
                              ] )
                    | exn -> Lwt.fail exn))
          | Mode_server -> (
              let promise = to_html_many this mode children in
              match Lwt.state promise with
              | Sleep ->
                  let idx = use_idx ctx in
                  ctx.waiting <- ctx.waiting + 1;
                  Lwt.async (fun () ->
                      promise >>= fun (html, json) ->
                      this >|= fun () ->
                      let html =
                        sprintf {|<div hidden id="S:%i">%s</div>|} idx
                          html
                      in
                      let hydrate =
                        sprintf {|<script>%s$RC("B:%i", "S:%i")</script>|}
                          instruction_RC idx idx
                      in
                      ctx.push
                        (Some
                           ( html ^ hydrate,
                             (idx, Yojson.Safe.to_string json) ));
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push None);
                  Lwt.return
                    ( sprintf
                        {|<!--$?--><template id="B:%i"></template><!--/$-->|}
                        idx,
                      `List
                        [
                          `String "$";
                          `String "$Sreact.suspense";
                          `Null;
                          `Assoc
                            [ "children", `String (sprintf "$L%i" idx) ];
                        ] )
              | Return (html, json) ->
                  Lwt.return
                    ( html,
                      `List
                        [
                          `String "$";
                          `String "$Sreact.suspense";
                          `Null;
                          `Assoc [ "children", json ];
                        ] )
              | Fail exn -> Lwt.fail exn))
      | El_thunk f ->
          let tree = f () in
          to_html this mode tree
      | El_async_thunk f -> (
          match mode with
          | Mode_client -> failwith "async component in client mode"
          | Mode_server -> f () >>= to_html this mode)
      | El_client_thunk { import_module; import_name; props; thunk } ->
          to_html this Mode_client thunk >>= fun (html, _model) ->
          let idx = use_idx ctx in
          let ref =
            `Assoc
              [
                "id", `String import_module;
                "name", `String import_name;
                "chunks", `List [];
                "async", `Bool false;
              ]
          in
          ctx.push
            (Some ("", (idx, sprintf "I%s" (Yojson.Safe.to_string ref))));
          Lwt_list.map_p (json_model_to_model this mode) props
          >|= fun props ->
          let model =
            `List
              [
                `String "$";
                `String (sprintf "$%i" idx);
                `Null;
                `Assoc props;
              ]
          in
          html, model
    and to_html_many this mode els =
      Array.to_list els
      |> Lwt_list.map_p (to_html this mode)
      >|= List.split
      >|= fun (html, model) ->
      String.concat ~sep:"<!-- -->" html, `List model
    and json_model_to_model this mode (name, jsony) =
      match jsony with
      | `Element element ->
          to_html this mode element >|= fun (_html, model) -> name, model
      | #json as jsony -> Lwt.return (name, (jsony :> json))
    in
    let this, ready = Lwt.wait () in
    let idx = ctx.idx in
    to_html this mode el >|= fun (html, json) ->
    ctx.push (Some (html, (idx, Yojson.Safe.to_string json)));
    Lwt.wakeup ready ();
    ctx.waiting <- ctx.waiting - 1;
    if ctx.waiting = 0 then ctx.push None

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx = { push; waiting = 1; idx = 0 } in
    to_html ctx Mode_server el >>= fun () ->
    Lwt_stream.iter_s on_chunk rendering
end

let emit_model idx model =
  sprintf {|<script>window.SSRPush(%i, %S)</script>|} idx model

let render_to_html ?(scripts = []) ?(links = []) f : Dream.handler =
 fun req ->
  let links =
    List.map links
      ~f:(sprintf {|<link href=%S type="text/css" rel="stylesheet" />|})
    |> String.concat ~sep:"\n"
  in
  let scripts =
    List.map scripts ~f:(sprintf {|<script src=%S></script>|})
    |> String.concat ~sep:"\n"
  in
  match Dream.header req "accept" with
  | Some "application/react.component" ->
      Dream.stream (fun stream ->
          Render_to_model.render (f req) @@ fun data ->
          Dream.write stream (Buffer.contents data) >>= fun () ->
          Dream.flush stream)
  | _ ->
      Dream.stream (fun stream ->
          Dream.write stream
            {|<script>
            let encoder = new TextEncoder();
            window.SSR = true;
            window.SSRPush = (idx, data) => {
              window.c.enqueue(encoder.encode(`${idx}:${data}\n`));
            };
            window.SSRClose = () => {
              window.c.close();
            };
            window.SSRStream = new ReadableStream({
              start(controller) {
                window.c = controller;
              },
            });
          </script>|}
          >>= fun () ->
          Dream.write stream (sprintf {|%s%s|} links scripts)
          >>= fun () ->
          Render_to_html.render (f req) (fun (html, (idx, json)) ->
              Dream.write stream (sprintf "%s\n" (emit_model idx json))
              >>= fun () ->
              Dream.write stream (sprintf "%s\n" html) >>= fun () ->
              (* Dream.write stream (sprintf "%i:%s\n" idx data) >>= fun () -> *)
              Dream.flush stream)
          >>= fun () ->
          Dream.write stream "<script>window.SSRClose();</script>")

let render ?(scripts = []) ?(links = []) f : Dream.handler =
 fun req ->
  match Dream.header req "accept" with
  | Some "application/react.component" ->
      Dream.stream (fun stream ->
          Render_to_model.render (f req) @@ fun data ->
          Dream.write stream (Buffer.contents data) >>= fun () ->
          Dream.flush stream)
  | _ ->
      let links =
        List.map links
          ~f:
            (sprintf {|<link href=%S type="text/css" rel="stylesheet" />|})
        |> String.concat ~sep:"\n"
      in
      let scripts =
        List.map scripts ~f:(sprintf {|<script src=%S></script>|})
        |> String.concat ~sep:"\n"
      in
      Dream.html (sprintf {|%s%s|} links scripts)
