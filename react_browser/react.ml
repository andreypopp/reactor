type element
type children = element array

external unsafe_create_element : ('props -> element) -> 'props -> element
  = "createElement"
[@@bs.module "react"]

external unsafe_create_element' : 'any -> 'props -> element
  = "createElement"
[@@bs.module "react"]

type suspense

external suspense_t : suspense = "Suspense" [@@bs.module "react"]

external suspense_create_element :
  suspense -> 'props -> element array -> element = "createElement"
[@@bs.module "react"] [@@bs.variadic]

let suspense ?fallback:_ children =
  suspense_create_element suspense_t Js.null children

let null = Obj.magic Js.null

external text : string -> element = "%identity"

type html_props

external html_props :
  ?key:string ->
  ?className:string ->
  ?children:children ->
  unit ->
  html_props = ""
[@@bs.obj]

external html' : string -> html_props -> element array -> element
  = "createElement"
[@@bs.module "react"] [@@bs.variadic]

type html_element = ?className:string -> children -> element

let h name : html_element =
 fun ?className children -> html' name (html_props ?className ()) children

let html = h "html"
let body = h "body"
let head = h "head"
let title = h "title"
let div = h "div"
let span = h "span"
let a = h "a"
let ol = h "ol"
let ul = h "ul"
let li = h "li"
let h1 = h "h1"
let h2 = h "h2"
let h3 = h "h3"
let h4 = h "h4"
let h5 = h "h5"
let h6 = h "h6"

external render_to_string : element -> string = "renderToString"
[@@bs.module "react-dom/server"]

external use_effect' : (unit -> unit) -> 'a array -> unit = "useEffect"
[@@bs.module "react"]

external use_effect : (unit -> unit -> unit) -> 'a array -> unit
  = "useEffect"
[@@bs.module "react"]

type 'a promise = 'a Promise.promise

external use : 'a promise -> 'a = "use" [@@bs.module "react"]
