(* module React = React *)
(* module Event = React_browser_event *)
(* module Html_props = React_browser_html_props *)
open Realm

module React = struct
  include React

  external use : 'a Promise.t -> 'a = "use" [@@mel.module "react"]

  external startTransition : (unit -> unit) -> unit = "startTransition"
  [@@mel.module "react"]

  external startTransitionAsync : (unit -> unit Promise.t) -> unit
    = "startTransition"
  [@@mel.module "react"]

  external useOptimistic :
    'state -> ('state -> 'update -> 'state) -> 'state * ('update -> unit)
    = "useOptimistic"
  [@@mel.module "react"]
end

module ReactDOM = struct
  include ReactDOM

  module Ref = struct
    include ReactDOM.Ref

    let useCurrentDomRef () : currentDomRef * domRef =
      let ref = React.useRef Js.Nullable.null in
      let dom_ref = ReactDOM.Ref.domRef ref in
      ref, dom_ref
  end
end

module ReactServerDOM = struct
  external createFromFetch :
    Fetch.response Promise.t -> React.element Promise.t
    = "createFromFetch"
  [@@mel.module "react-server-dom-webpack/client"]
end

module Component_map = React_browser_component_map

module Router = struct
  external navigate : string -> unit = "React_of_caml_navigate"
end
