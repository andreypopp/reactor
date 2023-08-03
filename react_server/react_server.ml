open ContainersLabels
open Monomorphic
open Printf
open Lwt.Infix

type json = Yojson.Safe.t
type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise

module React = struct
  type element =
    | El_null : element
    | El_suspense : {
        children : children;
        fallback : children;
      }
        -> element
    | El_text : string -> element
    | El_many : children -> element
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
  let many els = El_many els
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
    (* let children = *)
    (*   match children, dangerously_set_inner_html with *)
    (*   | children, None -> Some children *)
    (*   | [||], Some _ -> None *)
    (*   | _, Some _ -> *)
    (*       failwith *)
    (* "cannot have children and dangerously_set_inner_html at the \ *)
       (*          same time" *)
    (* in *)
    let props =
      let string v = `String v in
      (* let dangerously_set_inner_html = *)
      (*   Option.map *)
      (*     (fun html -> `Assoc [ "__html", `String html ]) *)
      (*     dangerously_set_inner_html *)
      (* in *)
      [
        "className", Option.map string className;
        (* "href", Option.map string href; *)
        (* "dangerouslySetInnerHTML", dangerously_set_inner_html; *)
      ]
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
  let use _promise = assert false
end

module React_browser = struct
  module React = struct
    include React

    let use_effect _thunk _deps = ()
    let use_effect' _thunk _deps = ()
    let use promise = raise (Suspend (Any_promise promise))
  end
end

module Render_to_model = struct
  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    push : (int * string) option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  let rec to_model ctx el =
    let rec to_model = function
      | React.El_null -> `Null
      | El_text s -> `String s
      | El_many els -> `List (Array.to_list els |> List.map ~f:to_model)
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
                   `List (Array.to_list children |> List.map ~f:to_model)
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
                    `List (Array.to_list children |> List.map ~f:to_model)
                  );
                ];
            ]
      | El_thunk f ->
          let tree = f () in
          to_model tree
      | El_async_thunk f -> (
          let tree = f () in
          match Lwt.state tree with
          | Lwt.Return tree -> to_model tree
          | Lwt.Fail exn -> raise exn
          | Lwt.Sleep ->
              let idx = use_idx ctx in
              ctx.waiting <- ctx.waiting + 1;
              Lwt.async (fun () ->
                  Lwt.map
                    (fun tree ->
                      let json = to_model tree in
                      ctx.push (Some (idx, Yojson.Safe.to_string json));
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push None)
                    tree);
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
          ctx.push (Some (idx, sprintf "I%s" (Yojson.Safe.to_string ref)));
          let props = List.map props ~f:(json_model_to_model ctx) in
          `List
            [
              `String "$";
              `String (sprintf "$%i" idx);
              `Null;
              `Assoc props;
            ]
    in
    to_model el

  and json_model_to_model ctx (name, jsony) =
    match jsony with
    | `Element element -> name, to_model ctx element
    | #json as jsony -> name, (jsony :> json)

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx = { push; waiting = 0; idx = 0 } in
    let idx = ctx.idx in
    ctx.push (Some (idx, Yojson.Safe.to_string (to_model ctx el)));
    if ctx.waiting = 0 then ctx.push None;
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

  type mode = S | C

  let instruction_RC =
    {|
    $RC=function(b,c,e){c=document.getElementById(c);c.parentNode.removeChild(c);var a=document.getElementById(b);if(a){b=a.previousSibling;if(e)b.data="$!",a.setAttribute("data-dgst",e);else{e=b.parentNode;a=b.nextSibling;var f=0;do{if(a&&8===a.nodeType){var d=a.data;if("/$"===d)if(0===f)break;else f--;else"$"!==d&&"$?"!==d&&"$!"!==d||f++}d=a.nextSibling;e.removeChild(a);a=d}while(a);for(;c.firstChild;)e.insertBefore(c.firstChild,a);b.data="$"}b._reactRetry&&b._reactRetry()}};
    |}

  let rec to_html ctx el =
    let rec to_html mode = function
      | React.El_null -> Lwt.return ("", `Null)
      | El_text s -> Lwt.return (s, `String s)
      | El_many els -> to_html_many mode els
      | El_html (tag_name, props, None) ->
          Lwt.return
            ( emit_html tag_name props None,
              `List [ `String "$"; `String tag_name; `Null; `Assoc props ]
            )
      | El_html (tag_name, props, Some children) ->
          to_html_many mode children >|= fun (html, model) ->
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
          | C ->
              let restart () = to_html_many mode children in
              let rec wait (Any_promise promise) =
                promise >>= fun _ ->
                Lwt.catch restart (function
                  | Suspend any_promise -> wait any_promise
                  | exn -> Lwt.fail exn)
              in
              Lwt.catch restart (function
                | Suspend any_promise ->
                    let idx = use_idx ctx in
                    ctx.waiting <- ctx.waiting + 1;
                    Lwt.async (fun () ->
                        wait any_promise >|= fun (html, json) ->
                        let html =
                          sprintf {|<div hidden id="S:%i">%s</div>|} idx
                            html
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
                        `String (sprintf "$L%i" idx) )
                | exn -> Lwt.fail exn)
          | S -> to_html_many mode children)
      | El_thunk f ->
          let tree = f () in
          to_html mode tree
      | El_async_thunk f -> (
          match mode with
          | C -> failwith "async component in client mode"
          | S -> f () >>= to_html mode)
      | El_client_thunk { import_module; import_name; props; thunk } ->
          to_html C thunk >>= fun (html, _model) ->
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
          Lwt_list.map_p (json_model_to_model ctx mode) props
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
    and to_html_many mode els =
      Array.to_list els |> Lwt_list.map_p (to_html mode) >|= List.split
      >|= fun (html, model) ->
      String.concat ~sep:"<!-- -->" html, `List model
    in
    to_html el

  and json_model_to_model ctx mode (name, jsony) =
    match jsony with
    | `Element element ->
        to_html ctx mode element >|= fun (_html, model) -> name, model
    | #json as jsony -> Lwt.return (name, (jsony :> json))

  let render el on_chunk =
    let rendering, push = Lwt_stream.create () in
    let ctx = { push; waiting = 0; idx = 0 } in
    let idx = ctx.idx in
    to_html ctx S el >>= fun (html, json) ->
    ctx.push (Some (html, (idx, Yojson.Safe.to_string json)));
    if ctx.waiting = 0 then ctx.push None;
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
          Render_to_model.render (f req) @@ fun (idx, data) ->
          Dream.write stream (sprintf "%i:%s\n" idx data) >>= fun () ->
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

let esbuild ?(sourcemap = false) entries : Dream.handler =
 fun _ ->
  (* TODO: stream from esbuild to response instead *)
  let args =
    [
      Some "esbuild";
      (if sourcemap then Some "--sourcemap" else None);
      Some "--bundle";
      Some "--loader:.js=jsx";
    ]
  in
  Lwt_process.pread
    ("esbuild", List.filter_map ~f:Fun.id args @ entries |> Array.of_list)
  >>= fun data ->
  Dream.respond data ~headers:[ "Content-type", "text/javascript" ]

let render ?(scripts = []) ?(links = []) f : Dream.handler =
 fun req ->
  match Dream.header req "accept" with
  | Some "application/react.component" ->
      Dream.stream (fun stream ->
          Render_to_model.render (f req) @@ fun (idx, data) ->
          Dream.write stream (sprintf "%i:%s\n" idx data) >>= fun () ->
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
