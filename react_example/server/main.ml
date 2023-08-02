open Printf
open Lwt.Infix

module UI = struct
  open React_server
  open React

  module Card = struct
    let make ~delay ~title children =
      async_thunk @@ fun () ->
      Lwt_unix.sleep delay >|= fun () ->
      div [| h1 [| text title |]; div children |]
  end

  let app _req =
    div ~className:"sans-serif h-100"
      [|
        h1 [| textf "React_server" |];
        Example_native.Example.App.make
          { title = "Title"; children = text "CHILDREN" };
        suspense
          [|
            Card.make ~title:"Sample Card 1" ~delay:1. [| text "HELLO" |];
          |];
        suspense
          [|
            Card.make ~title:"Sample Card 1" ~delay:2. [| text "HELLO" |];
          |];
        suspense
          [|
            Card.make ~title:"Sample Card 1" ~delay:3. [| text "HELLO" |];
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
       ]
