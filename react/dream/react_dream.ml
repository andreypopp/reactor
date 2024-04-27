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
        Dream.stream (fun stream ->
            render_to_model ui (fun data ->
                Dream.write stream data >>= fun () -> Dream.flush stream))
    | _ ->
        if enable_ssr then
          render_to_html ~render_model:enable_client_components ui
          >>= function
          | Html_rendering_done { html } ->
              Dream.html
                Html.(
                  splice [ html_prelude; html; html_scripts ] |> to_string)
          | Html_rendering_async { html_shell; html_iter } ->
              Dream.stream (fun stream ->
                  let write_and_flush html =
                    Dream.write stream (Html.to_string html) >>= fun () ->
                    Dream.flush stream
                  in
                  write_and_flush
                    Html.(
                      splice [ html_prelude; html_shell; html_scripts ])
                  >>= fun () -> html_iter write_and_flush)
        else
          Dream.html
            Html.(splice [ html_prelude; html_scripts ] |> to_string)
