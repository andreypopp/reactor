open ContainersLabels
open Lwt.Infix
open React_server

let html_prelude ~scripts ~links =
  let open Html in
  let make_link href =
    node "link" [ "href", s href; "rel", s "stylesheet" ] None
  in
  let make_script src = node "script" [ "src", s src ] (Some []) in
  splice ~sep:"\n"
    [
      Html.raw "<!doctype html>";
      Html.(node "meta" [ "charset", s "utf-8" ] None);
      List.map links ~f:make_link |> Html.splice ~sep:"\n";
      List.map scripts ~f:make_script |> Html.splice ~sep:"\n";
    ]

let rsc_content_type = "application/react.component"

let render ?(enable_ssr = true) ?(scripts = []) ?(links = []) =
  let html_prelude = html_prelude ~links ~scripts |> Html.to_string in
  fun f : Dream.handler ->
    fun req ->
     match Dream.header req "accept" with
     | Some accept when String.equal accept rsc_content_type ->
         Dream.stream (fun stream ->
             render_to_model (f req) (fun data ->
                 Dream.write stream data >>= fun () -> Dream.flush stream))
     | _ ->
         if enable_ssr then
           Dream.stream (fun stream ->
               Dream.write stream html_prelude >>= fun () ->
               render_to_html (f req) (fun data ->
                   Dream.write stream data >>= fun () ->
                   Dream.write stream "\n" >>= fun () ->
                   Dream.flush stream))
         else Dream.html html_prelude
