open ContainersLabels
open Lwt.Infix
open React_server

let make_script src =
  Html.(node "script" [ "src", s src; "async", b true ] [])

let make_link href =
  Html.(node "link" [ "href", s href; "rel", s "stylesheet" ] [])

let html_prelude ~links =
  Html.(
    splice ~sep:"\n"
      [
        unsafe_raw "<!doctype html>";
        node "meta" [ "charset", s "utf-8" ] [];
        List.map links ~f:make_link |> splice ~sep:"\n";
      ])

let rsc_content_type = "application/react.component"

module Chunked = struct
  type stream = { s : Dream.stream; is_len_encoded : bool }

  let write ?(flush = false) { s; is_len_encoded } data =
    (if is_len_encoded then
       let len = String.length data in
       let len = Printf.sprintf "%x\r\n" len in
       Dream.write s len >>= fun () ->
       Dream.write s data >>= fun () -> Dream.write s "\r\n"
     else Dream.write s data)
    >>= fun () -> if flush then Dream.flush s else Lwt.return_unit

  let finish { s; is_len_encoded } =
    (if is_len_encoded then Dream.write s "0\r\n\r\n" else Lwt.return ())
    >>= fun () -> Dream.flush s

  let stream ?headers ~is_len_encoded f =
    Dream.stream ?headers (fun s ->
        let s = { s; is_len_encoded } in
        f s >>= fun () -> finish s)
end

let render ?(enable_client_components = false) ?(enable_ssr = true)
    ?(scripts = []) ?(links = []) =
  let html_prelude = html_prelude ~links in
  let html_scripts =
    Html.(List.map scripts ~f:make_script |> splice ~sep:"\n")
  in
  fun ui req ->
    match Dream.header req "accept" with
    | Some accept
      when enable_client_components
           && String.equal accept rsc_content_type ->
        Chunked.stream ~is_len_encoded:true @@ fun s ->
        render_to_model ui (Chunked.write ~flush:true s)
    | _ ->
        if enable_ssr then
          render_to_html ~render_model:enable_client_components ui
          >>= function
          | Html_rendering_done { html } ->
              Dream.html
                Html.(
                  splice [ html_prelude; html; html_scripts ] |> to_string)
          | Html_rendering_async { html_shell; html_iter } ->
              let header =
                Html.(splice [ html_prelude; html_shell; html_scripts ])
              in
              Chunked.stream ~is_len_encoded:false
                ~headers:[ "Content-Type", "text/html" ]
              @@ fun s ->
              let write_html h =
                Chunked.write ~flush:true s (Html.to_string h)
              in
              write_html header >>= fun () -> html_iter write_html
        else
          Dream.html
            Html.(splice [ html_prelude; html_scripts ] |> to_string)
