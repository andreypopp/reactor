module React = React
module Component_map = Component_map

let () =
  (* we don't want to hide React behind react_api.REACT to hide external
     declations (allows melange to generate code directly using JS
     primitives/modules), instead we are just check that React subsumes
     React_api.REACT here *)
  let module R : React_api.REACT = React in
  ()
