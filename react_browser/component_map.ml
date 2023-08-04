type exported_component =
  | Exported_component : ('props -> React.element) -> exported_component
[@@unboxed]

type t = exported_component Js.Dict.t

let t = Js.Dict.empty ()

external window : t Js.Dict.t = "window" [@@bs.val]

let () = Js.Dict.set window "__exported_components" t

let register name component =
  Js.Dict.set t name (Exported_component component)
