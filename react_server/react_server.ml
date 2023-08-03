open ContainersLabels
open Monomorphic
open Printf
open Lwt.Infix

type json = Yojson.Safe.t

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

  let html tag_name : html_element =
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
                      ctx.push_rendering
                        (Some (idx, Yojson.Safe.to_string json));
                      ctx.waiting <- ctx.waiting - 1;
                      if ctx.waiting = 0 then ctx.push_rendering None)
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
          ctx.push_rendering
            (Some (idx, sprintf "I%s" (Yojson.Safe.to_string ref)));
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
    let rendering, push_rendering = Lwt_stream.create () in
    let ctx = { rendering; push_rendering; waiting = 0; idx = 0 } in
    let idx = ctx.idx in
    ctx.push_rendering
      (Some (idx, Yojson.Safe.to_string (to_model ctx el)));
    if ctx.waiting = 0 then ctx.push_rendering None;
    Lwt_stream.iter_s on_chunk ctx.rendering
end

(* module Render_to_html = struct *)
(*   type ctx = { *)
(*     mutable idx : int; *)
(*     mutable waiting : int; *)
(*     rendering : (int * string) Lwt_stream.t; *)
(*     push_rendering : (int * string) option -> unit; *)
(*   } *)

(*   let use_idx ctx = *)
(*     ctx.idx <- ctx.idx + 1; *)
(*     ctx.idx *)

(*   let rec to_html ctx el = *)
(*     let rec to_html = function *)
(*       | React.El_null -> `Null *)
(*       | El_text s -> `String s *)
(*       | El_many els -> `List (Array.to_list els |> List.map ~f:to_html) *)
(*       | El_html (tag_name, props, None) -> *)
(*           `List [ `String "$"; `String tag_name; `Null; `Assoc props ] *)
(*       | El_html (tag_name, props, Some children) -> *)
(*           `List *)
(*             [ *)
(*               `String "$"; *)
(*               `String tag_name; *)
(*               `Null; *)
(*               `Assoc *)
(*                 (( "children", *)
(*                    `List (Array.to_list children |> List.map ~f:to_html) *)
(*                  ) *)
(*                 :: props); *)
(*             ] *)
(*       | El_suspense { children; fallback = _ } -> *)
(*           `List *)
(*             [ *)
(*               `String "$"; *)
(*               `String "$Sreact.suspense"; *)
(*               `Null; *)
(*               `Assoc *)
(*                 [ *)
(*                   ( "children", *)
(*                     `List (Array.to_list children |> List.map ~f:to_html) *)
(*                   ); *)
(*                 ]; *)
(*             ] *)
(*       | El_thunk f -> *)
(*           let tree = f () in *)
(*           to_html tree *)
(*       | El_async_thunk f -> ( *)
(*           let tree = f () in *)
(*           match Lwt.state tree with *)
(*           | Lwt.Return tree -> to_html tree *)
(*           | Lwt.Fail exn -> raise exn *)
(*           | Lwt.Sleep -> *)
(*               let idx = use_idx ctx in *)
(*               ctx.waiting <- ctx.waiting + 1; *)
(*               Lwt.async (fun () -> *)
(*                   Lwt.map *)
(*                     (fun tree -> *)
(*                       let json = to_html tree in *)
(*                       ctx.push_rendering *)
(*                         (Some (idx, Yojson.Safe.to_string json)); *)
(*                       ctx.waiting <- ctx.waiting - 1; *)
(*                       if ctx.waiting = 0 then ctx.push_rendering None) *)
(*                     tree); *)
(*               `String (sprintf "$L%i" idx)) *)
(*       | El_client_thunk *)
(*           { import_module = _; import_name = _; props = _; thunk } -> *)
(*           to_html thunk *)
(*     in *)
(*     to_html el *)

(*   let render el on_chunk = *)
(*     let rendering, push_rendering = Lwt_stream.create () in *)
(*     let ctx = { rendering; push_rendering; waiting = 0; idx = 0 } in *)
(*     let idx = ctx.idx in *)
(*     ctx.push_rendering *)
(*       (Some (idx, Yojson.Safe.to_string (to_html ctx el))); *)
(*     if ctx.waiting = 0 then ctx.push_rendering None; *)
(*     Lwt_stream.iter_s on_chunk ctx.rendering *)
(* end *)

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
      Dream.html (sprintf {|%s<div id="root">%s|} links scripts)
