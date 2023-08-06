open ContainersLabels
open Lwt.Infix
open React_server

let make_script src =
  Html.(node "script" [ "src", s src; "async", b true ] (Some []))

let make_link href =
  Html.(node "link" [ "href", s href; "rel", s "stylesheet" ] None)

let html_prelude ~links =
  Html.(
    splice ~sep:"\n"
      [
        raw "<!doctype html>";
        node "meta" [ "charset", s "utf-8" ] None;
        List.map links ~f:make_link |> splice ~sep:"\n";
      ])

let rsc_content_type = "application/react.component"

let render ?(enable_ssr = true) ?(scripts = []) ?(links = []) =
  let html_prelude = html_prelude ~links |> Html.to_string in
  let scripts =
    Html.(
      List.map scripts ~f:make_script |> splice ~sep:"\n" |> to_string)
  in
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
               let on_shell_ready () = Dream.write stream scripts in
               render_to_html ~on_shell_ready (f req) (fun data ->
                   Dream.write stream data >>= fun () ->
                   Dream.write stream "\n" >>= fun () ->
                   Dream.flush stream))
         else Dream.html html_prelude
