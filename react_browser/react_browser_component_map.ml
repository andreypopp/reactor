type exported_component = string Js.Dict.t -> React.element
type t = exported_component Js.Dict.t

let t = Js.Dict.empty ()

external window : t Js.Dict.t = "window"

let () = Js.Dict.set window "__exported_components" t
let register name render = Js.Dict.set t name render
