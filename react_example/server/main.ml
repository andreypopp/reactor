[@@@warning "-32"]

open! ContainersLabels
open! Monomorphic
open Lwt.Infix

module UI = struct
  open React_server
  open React

  let%async_component card ~delay ~title children =
    Lwt_unix.sleep delay >|= fun () ->
    div ~className:"ba pa2"
      [|
        h3 ~className:"ma0 pa0 pb2" [| text title |];
        div ~className:"pb2" children;
        div ~className:"f7 bt pa1"
          [|
            textf "I've been sleeping for %0.1fsec before appearing."
              delay;
          |];
      |]

  let%component page ~title:title' children =
    html
      [|
        head [| title [| text title' |] |];
        body ~className:"pa4 sans-serif h-100"
          [| h1 [| text title' |]; div children |];
      |]

  let app _req =
    page ~title:"React with native React Server Components"
      [|
        div ~className:"flex flex-column g2"
          [|
            Example_native.Example.App.make
              {
                title = "Hello from Client Component";
                children =
                  text "As you can see, this one is SSR'ed as well.";
              };
            card ~title:"Initial Data" ~delay:0.
              [|
                text
                  "This components loads some async data but will block \
                   the shell until this data is ready.";
              |];
            suspense
              [|
                card ~title:"Async Data" ~delay:1. [| text "HELLO" |];
                card ~title:"Async Data" ~delay:1. [| text "HELLO" |];
              |];
            suspense
              [|
                card ~title:"Async Data" ~delay:2. [| text "OUTER" |];
                suspense
                  [|
                    card ~title:"Inner Async Data" ~delay:1.
                      [| text "INNER" |];
                  |];
              |];
            div
              [|
                h2 [| text "Testing XSS" |];
                ul
                  [|
                    li
                      [|
                        text "</script><script>console.log(1)</script>";
                      |];
                    li
                      [| text "\u{2028}<script>console.log(1)</script>" |];
                  |];
              |];
          |];
      |]
end

let () =
  let static =
    Static.Sites.static
    |> List.head_opt
    |> Option.get_exn_or "no /static dir found"
  in
  let links = [ "/static/bundle.css" ] in
  let scripts = [ "/static/bundle.js" ] in
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/static/**" (Dream.static static);
         Dream.get "/"
           (React_dream.render ~enable_ssr:false ~links ~scripts UI.app);
         Dream.get "/ssr" (React_dream.render ~links ~scripts UI.app);
       ]
