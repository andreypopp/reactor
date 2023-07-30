open Lwt.Infix

module UI = struct
  open React_server_components
  open React_element

  module Card = struct
    let make ~delay ~title children =
      async_thunk @@ fun () ->
      Lwt_unix.sleep delay >|= fun () ->
      div [ h1 [ text title ]; div children ]
  end

  module Time = struct
    let make label = client_thunk "Time" [ "label", `Element label ]
  end

  let app _req =
    div ~className:"sans-serif h-100"
      [
        h1 [ textf "React_server_components" ];
        Time.make (text "Current time is");
        suspense
          [ Card.make ~title:"Sample Card 1" ~delay:1. [ text "HELLO" ] ];
        suspense
          [ Card.make ~title:"Sample Card 1" ~delay:2. [ text "HELLO" ] ];
        suspense
          [ Card.make ~title:"Sample Card 1" ~delay:3. [ text "HELLO" ] ];
      ]
end

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/runtime.js"
           (React_server_components.esbuild "./runtime.js" ~sourcemap:true);
         Dream.get "/" (React_server_components.render UI.app);
       ]
