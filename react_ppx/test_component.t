The form `let%component name ...args = body` allows to define a React component
using the same syntax for both browser and native environments:

  $ ./ppx_test_runner <<EOF
  > let%component name children =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name children = BODY in
    let name props = name props##children in
    fun ?key children ->
      React.unsafe_create_element name [%mel.obj { key; children }]
  
  (* NATIVE *)
  let name ?key children =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments are supported:

  $ ./ppx_test_runner <<EOF
  > let%component name ~label ?optional ?(with_default=1) =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name ~label ?optional ?(with_default = 1) = BODY in
    let name props =
      name ~label:props##label ?optional:props##optional
        ?with_default:props##with_default
    in
    fun ?key ~label ?optional ?with_default ->
      React.unsafe_create_element name
        [%mel.obj { key; label; optional; with_default }]
  
  (* NATIVE *)
  let name ?key ~label ?optional ?(with_default = 1) =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support aliasing label to another:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:alias ?optional:opt =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name ~labeled:alias ?optional:opt = BODY in
    let name props =
      name ~labeled:props##labeled ?optional:props##optional
    in
    fun ?key ~labeled ?optional ->
      React.unsafe_create_element name [%mel.obj { key; labeled; optional }]
  
  (* NATIVE *)
  let name ?key ~labeled:alias ?optional:opt =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Labeled/optional arguments support destructuring pattern matching:

  $ ./ppx_test_runner <<EOF
  > let%component name ~labeled:{name;value} ?optional:(Some {opt}) =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name ~labeled:{ name; value } ?optional:(Some { opt }) = BODY in
    let name props =
      name ~labeled:props##labeled ?optional:props##optional
    in
    fun ?key ~labeled ?optional ->
      React.unsafe_create_element name [%mel.obj { key; labeled; optional }]
  
  (* NATIVE *)
  let name ?key ~labeled:{ name; value } ?optional:(Some { opt }) =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Unlabeled arguments support destructuring pattern matching:

  $ ./ppx_test_runner <<EOF
  > let%component name {name;value} () =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name { name; value } () = BODY in
    let name props = name props##prop_0 props##prop_1 in
    fun ?key prop_0 prop_1 ->
      React.unsafe_create_element name [%mel.obj { key; prop_0; prop_1 }]
  
  (* NATIVE *)
  let name ?key { name; value } () =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)

Patterns with type constraint:

  $ ./ppx_test_runner <<EOF
  > let%component name ~(label : label_type) (pos : pos_type) =
  >   BODY
  > EOF
  (* BROWSER *)
  let name =
    let name ~(label : label_type) (pos : pos_type) = BODY in
    let name props = name ~label:props##label props##pos in
    fun ?key ~label pos ->
      React.unsafe_create_element name [%mel.obj { key; label; pos }]
  
  (* NATIVE *)
  let name ?key ~(label : label_type) (pos : pos_type) =
    let _ = key in
    React_server.React.thunk (fun () -> BODY)
