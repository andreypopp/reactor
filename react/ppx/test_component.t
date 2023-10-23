The form `let%component name ...args = body` allows to define a React component
using the same syntax for both browser and native environments:

  $ ./ppx_test_runner <<EOF
  > let%component no_props () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let no_props () = BODY
    let no_props props = no_props ()
    let no_props__props ?key () = [%mel.obj { key }]
  end
  
  (* NATIVE *)
  let no_props ?key () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

  $ ./ppx_test_runner <<EOF
  > let%component name ~children () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let name ~children () = BODY
    let name props = name ~children:props##children ()
    let name__props ?key () = [%mel.obj { key; children = React.null }]
  end
  
  (* NATIVE *)
  let name ?key ~children () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments are supported:

  $ ./ppx_test_runner <<EOF
  > let%component name ~label ?optional ?(with_default=1) () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let name ~label ?optional ?(with_default = 1) () = BODY
  
    let name props =
      name ~label:props##label ?optional:props##optional
        ?with_default:props##with_default ()
  
    let name__props ?key ~label ?optional ?with_default () =
      [%mel.obj { key; label; optional; with_default }]
  end
  
  (* NATIVE *)
  let name ?key ~label ?optional ?(with_default = 1) () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support aliasing label to another:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:alias ?optional:opt () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let name ~labeled:alias ?optional:opt () = BODY
  
    let name props =
      name ~labeled:props##labeled ?optional:props##optional ()
  
    let name__props ?key ~labeled ?optional () =
      [%mel.obj { key; labeled; optional }]
  end
  
  (* NATIVE *)
  let name ?key ~labeled:alias ?optional:opt () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support destructuring pattern matching:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:{name;value} ?optional:(Some {opt}) () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let name ~labeled:{ name; value } ?optional:(Some { opt }) () = BODY
  
    let name props =
      name ~labeled:props##labeled ?optional:props##optional ()
  
    let name__props ?key ~labeled ?optional () =
      [%mel.obj { key; labeled; optional }]
  end
  
  (* NATIVE *)
  let name ?key ~labeled:{ name; value } ?optional:(Some { opt }) () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Patterns with type constraint:

  $ ./ppx_test_runner <<EOF
  > let%component name ~(label : label_type) () =
  >   BODY
  > EOF
  (* BROWSER *)
  include struct
    let name ~(label : label_type) () = BODY
    let name props = name ~label:props##label ()
    let name__props ?key ~label () = [%mel.obj { key; label }]
  end
  
  (* NATIVE *)
  let name ?key ~(label : label_type) () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)
