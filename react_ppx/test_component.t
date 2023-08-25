The form `let%component name ...args = body` allows to define a React component
using the same syntax for both browser and native environments:

  $ ./ppx_test_runner <<EOF
  > let%component name children =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name children = BODY in
    let name props = name props##children in
    fun children -> React.unsafe_create_element name [%mel.obj { children }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name children = React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments are supported:

  $ ./ppx_test_runner <<EOF
  > let%component name ~label ?optional ?(with_default=1) =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name ~label ?optional ?(with_default = 1) = BODY in
    let name props =
      name ~label:props##label ?optional:props##optional
        ?with_default:props##with_default
    in
    fun ~label ?optional ?with_default ->
      React.unsafe_create_element name
        [%mel.obj { label; optional; with_default }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name ~label ?optional ?(with_default = 1) =
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support aliasing label to another:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:alias ?optional:opt =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name ~labeled:alias ?optional:opt = BODY in
    let name props =
      name ~labeled:props##labeled ?optional:props##optional
    in
    fun ~labeled ?optional ->
      React.unsafe_create_element name [%mel.obj { labeled; optional }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name ~labeled:alias ?optional:opt =
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support destructuring pattern matching:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:{name;value} ?optional:(Some {opt}) =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name ~labeled:{ name; value } ?optional:(Some { opt }) = BODY in
    let name props =
      name ~labeled:props##labeled ?optional:props##optional
    in
    fun ~labeled ?optional ->
      React.unsafe_create_element name [%mel.obj { labeled; optional }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name ~labeled:{ name; value } ?optional:(Some { opt }) =
    React_server.React.thunk (fun () -> BODY)

Unlabeled arguments support destructuring pattern matching:

  $ ./ppx_test_runner <<EOF
  > let%component name {name;value} () =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name { name; value } () = BODY in
    let name props = name props##prop_0 props##prop_1 in
    fun prop_0 prop_1 ->
      React.unsafe_create_element name [%mel.obj { prop_0; prop_1 }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name { name; value } () = React_server.React.thunk (fun () -> BODY)

Patterns with type constraint:

  $ ./ppx_test_runner <<EOF
  > let%component name ~(label : label_type) (pos : pos_type) =
  >   BODY
  > EOF
  (* BROWSER *)
  open React_browser
  
  let name =
    let name ~(label : label_type) (pos : pos_type) = BODY in
    let name props = name ~label:props##label props##pos in
    fun ~label pos ->
      React.unsafe_create_element name [%mel.obj { label; pos }]
  
  (* NATIVE *)
  open React_server.React_browser
  
  let name ~(label : label_type) (pos : pos_type) =
    React_server.React.thunk (fun () -> BODY)
