The form `let%async_component name ...args = body` allows to define a React
component for use on server, use in browser is not allowed:

  $ ./ppx_test_runner <<EOF
  > let%async_component name children =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name = [%ocaml.error "async components are not supported in browser"]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name children = React_server.React.async_thunk (fun () -> BODY)

