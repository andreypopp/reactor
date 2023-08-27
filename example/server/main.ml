open! ContainersLabels
open! Monomorphic
open Lwt.Infix

module UI = struct
  open React_server

  let%async_component card ~delay ~title children =
    Lwt_unix.sleep delay >|= fun () ->
    jsx.div ~className:"ba pa2"
      [|
        jsx.h3 ~className:"ma0 pa0 pb2" [| React.text title |];
        jsx.div ~className:"pb2" children;
        jsx.div ~className:"f7 bt pa1"
          [|
            React.textf
              "I've been sleeping for %0.1fsec before appearing." delay;
          |];
      |]

  let%component page ~title children =
    jsx.html ~className:"h-100"
      [|
        jsx.head [| jsx.title [| React.text title |] |];
        jsx.body
          ~className:"pa4 sans-serif dark-gray bg-washed-yellow h-100"
          [|
            jsx.h1 [| jsx.span [| React.text title |] |]; jsx.div children;
          |];
      |]

  let app _req =
    page ~title:"React of OCaml"
      [|
        jsx.div ~className:"flex flex-column g2 measure-wide"
          [|
            Example.app ~title:"Hello from Client Component"
              (React.text "As you can see, this one is SSR'ed as well.");
            card ~title:"Initial Data" ~delay:0.
              [|
                React.text
                  "This components loads some async data but will block \
                   the shell until this data is ready.";
              |];
            React.suspense
              [|
                card ~title:"Async Data" ~delay:1.
                  [| React.text "HELLO" |];
                card ~title:"Async Data" ~delay:1.
                  [| React.text "HELLO" |];
              |];
            React.suspense
              [|
                card ~title:"Async Data" ~delay:2.
                  [| React.text "OUTER" |];
                React.suspense
                  [|
                    card ~title:"Inner Async Data" ~delay:1.
                      [| React.text "INNER" |];
                  |];
              |];
            jsx.div
              [|
                jsx.h2 [| React.text "Testing XSS" |];
                jsx.ul
                  [|
                    jsx.li
                      [|
                        React.text
                          "</script><script>console.log(1)</script>";
                      |];
                    jsx.li
                      [|
                        React.text
                          "\u{2028}<script>console.log(1)</script>";
                      |];
                  |];
              |];
          |];
      |]

  let about _req =
    page ~title:"About"
      [|
        jsx.div ~className:"flex flex-column g2 measure-wide"
          [|
            Example.about ~num:1 ~mode:About_light ();
            jsx.p [| React.text "Just an about page" |];
          |];
      |]

  let todos _req =
    page ~title:"TODOs"
      [|
        jsx.div ~className:"flex flex-column g2 measure-wide"
          [| Example.todo_list () |];
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
  let render ui = React_dream.render ~links ~scripts ui in
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       ([
          Dream.get "/static/**" (Dream.static static);
          Dream.get "/" (render UI.app);
          Dream.get "/about" (render UI.about);
          Dream.get "/todo" (render UI.todos);
          Dream.get "/no-ssr"
            (React_dream.render ~enable_ssr:false ~links ~scripts UI.app);
        ]
       @ Api.Hello.routes
       @ Api.Todo.routes)
