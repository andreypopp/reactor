open Printf
open Lwt.Infix

module UI = struct
  open React_server
  open React

  let%async_component card ~delay ~title children =
    Lwt_unix.sleep delay >|= fun () ->
    div [| h1 [| text title |]; div children |]

  let app _req =
    html
      [|
        head
          [|
            title [| text "React with native React Server Components" |];
          |];
        body
          [|
            div ~className:"sans-serif h-100"
              [|
                h1 [| textf "React_server" |];
                Example_native.Example.App.make
                  { title = "Title"; children = text "CHILDREN" };
                card ~title:"Some initial data (server will block)"
                  ~delay:0.
                  [| text "Initial data loaded!" |];
                suspense
                  [|
                    card ~title:"Sample Card 1" ~delay:1.
                      [| text "HELLO" |];
                  |];
                suspense
                  [|
                    card ~title:"Sample Card 2" ~delay:2.
                      [| text "HELLO" |];
                  |];
                suspense
                  [|
                    card ~title:"Sample Card 3" ~delay:3.
                      [| text "HELLO" |];
                  |];
              |];
          |];
      |]
end

let () =
  let project_root = Sys.getenv "OPAMSWITCH" in
  let dirname = Filename.dirname __FILE__ in
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/runtime.js"
           (Dream.from_filesystem
              (sprintf "%s/_build/default/%s/../browser" project_root
                 dirname)
              "bundle.js");
         Dream.get "/"
           (React_server.render ~scripts:[ "/runtime.js" ] UI.app);
         Dream.get "/ssr"
           (React_server.render_to_html ~scripts:[ "/runtime.js" ] UI.app);
       ]
