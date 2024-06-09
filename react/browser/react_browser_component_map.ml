type exported_component = Js.Json.t Js.Dict.t -> React.element
type t = exported_component Js.Dict.t

let t = Js.Dict.empty ()

external window : t Js.Dict.t = "window"

let () = Js.Dict.set window "__exported_components" t

let register name render =
  Js.log ("registering component: " ^ name);
  Js.Dict.set t name render
