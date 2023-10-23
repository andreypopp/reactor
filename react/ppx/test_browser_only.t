The %browser_only form allows to mark code as only to be compiled/executed for
a browser environment.

The first %browser_only form applies to expessions/structure items which match
againts () value:

  $ ./ppx_test_runner <<EOF
  > let%browser_only () = Js.log "OOPS"
  > let () =
  >   let%browser_only () = Js.log "OOPS" in
  >   ()
  > EOF
  (* BROWSER *)
  let () = Js.log "OOPS"
  
  let () =
    let () = Js.log "OOPS" in
    ()
  
  (* NATIVE *)
  let () = ()
  
  let () =
    let () = () in
    ()

The second %browser_only form applies to expression/structure items which
syntactically represent a function:

  $ ./ppx_test_runner <<EOF
  > let%browser_only f () = Js.log "OOPS"
  > let () =
  >   let%browser_only f () = Js.log "OOPS" in
  >   ()
  > 
  > let%browser_only f = function () -> Js.log "OOPS"
  > let () =
  >   let%browser_only f = function () -> Js.log "OOPS" in
  >   ()
  > EOF
  (* BROWSER *)
  let f () = Js.log "OOPS"
  
  let () =
    let f () = Js.log "OOPS" in
    ()
  
  let f = function () -> Js.log "OOPS"
  
  let () =
    let f = function () -> Js.log "OOPS" in
    ()
  
  (* NATIVE *)
  let f _ = raise React_server.React.Browser_only
  
  let () =
    let f _ = raise React_server.React.Browser_only in
    ()
  
  let f _ = raise React_server.React.Browser_only
  
  let () =
    let f _ = raise React_server.React.Browser_only in
    ()

Any other uses are disallowed:

  $ ./ppx_test_runner <<EOF
  > let%browser_only f = Js.log "OOPS"
  > let () =
  >   let%browser_only f = Js.log "OOPS" in
  >   ()
  > EOF
  (* BROWSER *)
  let f = Js.log "OOPS"
  
  let () =
    let f = Js.log "OOPS" in
    ()
  
  (* NATIVE *)
  let [%e ()] =
    [%ocaml.error
      "Invalid %browser_only usage, only the following is allowed:\n\
      \  let%browser_only () = ...\n\
      \  let%browser_only func arg1 ... = ..."]
  
  let () =
    [%ocaml.error
      "Invalid %browser_only usage, only the following is allowed:\n\
      \  let%browser_only () = ...\n\
      \  let%browser_only func arg1 ... = ..."]
