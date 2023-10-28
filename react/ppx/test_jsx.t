Producing HTML elements, no props:
  $ ./ppx_test_runner <<EOF
  > div ~children:[] () [@JSX]
  > EOF
  (* BROWSER *)
  ReactDOM.jsx "div" (ReactDOM.domProps ())
  
  (* NATIVE *)
  React_server.React.unsafe_create_html_element "div" [] None

Producing HTML elements, with props:
  $ ./ppx_test_runner <<EOF
  > div ~className:"a" () [@JSX]
  > EOF
  (* BROWSER *)
  ReactDOM.jsx "div" (ReactDOM.domProps ~className:"a" ())
  
  (* NATIVE *)
  React_server.React.unsafe_create_html_element "div"
    [ React_server.React.Html_props.className "a" ]
    None

Producing HTML elements, with children:
  $ ./ppx_test_runner <<EOF
  > div ~children:[a; div ~children:[] () [@JSX]] () [@JSX]
  > EOF
  (* BROWSER *)
  ReactDOM.jsxs "div"
    (ReactDOM.domProps
       ~children:
         (React.array [| a; ReactDOM.jsx "div" (ReactDOM.domProps ()) |])
       ())
  
  (* NATIVE *)
  React_server.React.unsafe_create_html_element "div" []
    (Some
       (React_server.React.Html_children
          [|
            a; React_server.React.unsafe_create_html_element "div" [] None;
          |]))

Producing HTML elements, with props with nested elements:
  $ ./ppx_test_runner <<EOF
  > div ~not_children:(div ~children:[] () [@JSX]) () [@JSX]
  > EOF
  (* BROWSER *)
  ReactDOM.jsx "div"
    (ReactDOM.domProps
       ~not_children:(ReactDOM.jsx "div" (ReactDOM.domProps ()))
       ())
  
  (* NATIVE *)
  React_server.React.unsafe_create_html_element "div"
    [
      React_server.React.Html_props.not_children
        (React_server.React.unsafe_create_html_element "div" [] None);
    ]
    None

Producing component elements, no props:
  $ ./ppx_test_runner <<EOF
  > some_thing ~children:[] () [@JSX]
  > EOF
  (* BROWSER *)
  React.jsx some_thing (some_thingProps ())
  
  (* NATIVE *)
  some_thing ()

Producing component elements, with props:
  $ ./ppx_test_runner <<EOF
  > some_thing ~className:"a" () [@JSX]
  > EOF
  (* BROWSER *)
  React.jsx some_thing (some_thingProps ~className:"a" ())
  
  (* NATIVE *)
  some_thing ~className:"a" ()

Producing component elements, with children:
  $ ./ppx_test_runner <<EOF
  > some_thing ~children:[a; div ~children:[] () [@JSX]] () [@JSX]
  > EOF
  (* BROWSER *)
  React.jsxs some_thing
    (some_thingProps
       ~children:
         (React.array [| a; ReactDOM.jsx "div" (ReactDOM.domProps ()) |])
       ())
  
  (* NATIVE *)
  some_thing
    ~children:
      [| a; React_server.React.unsafe_create_html_element "div" [] None |]
    ()

Producing component with (within module) elements, no props:
  $ ./ppx_test_runner <<EOF
  > Some.component ~children:[] () [@JSX]
  > EOF
  (* BROWSER *)
  React.jsx Some.component (Some.componentProps ())
  
  (* NATIVE *)
  Some.component ()
