type element

let yojson_of_element _ = assert false

external unsafe_create_element : ('props -> element) -> 'props -> element
  = "createElement"
[@@bs.module "react"]

external text : string -> element = "%identity"

type html_props

external html_props :
  ?key:string ->
  ?className:string ->
  ?children:element array ->
  unit ->
  html_props = ""
[@@bs.obj]

external html' : string -> html_props -> element array -> element
  = "createElement"
[@@bs.module "react"] [@@bs.variadic]

let html name ?key ?className children =
  html' name (html_props ?key ?className ()) children

let div = html "div"
let span = html "span"
let ol = html "ol"
let ul = html "ul"
let li = html "li"
let h1 = html "h1"
let h2 = html "h2"
let h3 = html "h3"
let h4 = html "h4"
let h5 = html "h5"
let h6 = html "h6"

external render_to_string : element -> string = "renderToString"
[@@bs.module "react-dom/server"]

module Exported_components = struct
  type exported_component =
    | Exported_component : ('props -> element) -> exported_component
  [@@unboxed]

  type t = exported_component Js.Dict.t

  let t = Js.Dict.empty ()

  external window : t Js.Dict.t = "window" [@@bs.val]

  let () = Js.Dict.set window "__exported_components" t

  let register name component =
    Js.Dict.set t name (Exported_component component)
end
