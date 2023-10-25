(* module React = React *)
(* module Event = React_browser_event *)
(* module Html_props = React_browser_html_props *)
open Realm

module React = struct
  include React

  external use : 'a Promise.t -> 'a = "use" [@@mel.module "react"]

  external startTransition : (unit -> unit) -> unit = "startTransition"
  [@@mel.module "react"]
end

module ReactDOM = struct
  include ReactDOM

  let useDomRef () =
    let ref = React.useRef Js.Nullable.null in
    let dom_ref = ReactDOM.Ref.domRef ref in
    ref, dom_ref
end

module Component_map = React_browser_component_map

module Router = struct
  external navigate : string -> unit = "React_of_caml_navigate"
end
