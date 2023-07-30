open ContainersLabels
open Monomorphic
open Printf
open Lwt.Infix

type json = Yojson.Safe.t

module React_element = struct
  type t =
    | El_null : t
    | El_text : string -> t
    | El_many : children -> t
    | El_html : string * html_props * children -> t
    | El_thunk : (unit -> t) -> t
    | El_async_thunk : (unit -> t Lwt.t) -> t
    | El_client_thunk : {
        import_module : string;
        import_name : string;
        props : (string * json_model) list;
      }
        -> t

  and children = t list
  and html_props = (string * json) list

  and json_model =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * json_model) list
    | `List of json_model list
    | `Tuple of json_model list
    | `Variant of string * json_model option
    | `Element of t ]

  let null = El_null
  let many els = El_many els
  let text s = El_text s
  let textf fmt = ksprintf text fmt
  let thunk f = El_thunk f
  let async_thunk f = El_async_thunk f
  let suspense children = El_html ("$Sreact.suspense", [], children)

  let client_thunk ?(import_name = "") import_module props =
    El_client_thunk { import_module; import_name; props }

  type html_element = ?className:string -> ?href:string -> children -> t

  let html tag_name : html_element =
   fun ?className ?href children ->
    let props =
      [ "className", className; "href", href ]
      |> List.filter_map ~f:(function
           | _, None -> None
           | n, Some v -> Some (n, `String v))
    in
    El_html (tag_name, props, children)

  let div = html "div"
  let a = html "a"
  let span = html "span"
  let li = html "li"
  let ul = html "ul"
  let ol = html "ol"
  let h1 = html "h1"
  let h2 = html "h2"
  let h3 = html "h3"
end

module Render_to_model = struct
  type ctx = {
    mutable idx : int;
    mutable waiting : int;
    rendering : (int * string) Lwt_stream.t;
    push_rendering : (int * string) option -> unit;
  }

  let use_idx ctx =
    ctx.idx <- ctx.idx + 1;
    ctx.idx

  let rec to_model ctx el =
    let rec to_model = function
      | React_element.El_null -> `Null
      | El_text s -> `String s
      | El_many els -> `List (List.map els ~f:to_model)
      | El_html (tag_name, props, children) ->
          `List
            [
              `String "$";
              `String tag_name;
              `Null;
              `Assoc
                (("children", `List (List.map ~f:to_model children))
                :: props);
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
                      ctx.push_rendering
                        (Some (idx, Yojson.Safe.to_string json));
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push_rendering None)
                    tree);
              `String (sprintf "$L%i" idx))
      | El_client_thunk { import_module; import_name; props } ->
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
          ctx.push_rendering
            (Some (idx, sprintf "I%s" (Yojson.Safe.to_string ref)));
          let props = json_model_to_model ctx (`Assoc props) in
          `List [ `String "$"; `String (sprintf "$%i" idx); `Null; props ]
    in
    to_model el

  and json_model_to_model ctx (m : React_element.json_model) : json =
    match m with
    | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as
      json ->
        json
    | `Assoc xs ->
        `Assoc
          (List.map xs ~f:(fun (k, v) -> k, json_model_to_model ctx v))
    | `List xs -> `List (List.map xs ~f:(json_model_to_model ctx))
    | `Tuple xs -> `Tuple (List.map xs ~f:(json_model_to_model ctx))
    | `Variant (name, None) -> `Variant (name, None)
    | `Variant (name, Some j) ->
        `Variant (name, Some (json_model_to_model ctx j))
    | `Element element -> to_model ctx element

  let render el on_chunk =
    let rendering, push_rendering = Lwt_stream.create () in
    let ctx = { rendering; push_rendering; waiting = 0; idx = 0 } in
    let idx = ctx.idx in
    ctx.push_rendering
      (Some (idx, Yojson.Safe.to_string (to_model ctx el)));
    if ctx.waiting = 0 then ctx.push_rendering None;
    Lwt_stream.iter_s on_chunk ctx.rendering
end

let esbuild ?(sourcemap = false) entry : Dream.handler =
 fun _ ->
  (* TODO: stream from esbuild to response instead *)
  let args =
    [
      Some "esbuild";
      (if sourcemap then Some "--sourcemap" else None);
      Some "--bundle";
      Some "--loader:.js=jsx";
      Some entry;
    ]
  in
  Lwt_process.pread
    ("esbuild", List.filter_map ~f:Fun.id args |> Array.of_list)
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
        let scripts = "/runtime.js" :: scripts in
        List.map scripts ~f:(sprintf {|<script src=%S></script>|})
        |> String.concat ~sep:"\n"
      in
      Dream.html (sprintf {|%s<div id="root">%s|} links scripts)
