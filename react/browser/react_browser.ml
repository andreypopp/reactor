module React = React
module Event = React_browser_event
module Html_props = React_browser_html_props
module Component_map = React_browser_component_map

external navigate : string -> unit = "React_of_caml_navigate"

let () =
  (* we don't want to hide React behind react_api.REACT to hide external
     declations (allows melange to generate code directly using JS
     primitives/modules), instead we are just check that React subsumes
     React_api.REACT here *)
  let module R : React_api.REACT = React in
  ()
