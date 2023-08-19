type element
type children = element array

external unsafe_create_element : ('props -> element) -> 'props -> element
  = "createElement"
[@@mel.module "react"]

type suspense

external suspense_t : suspense = "Suspense" [@@mel.module "react"]

external suspense_create_element :
  suspense -> 'props -> element array -> element = "createElement"
[@@mel.module "react"] [@@mel.variadic]

let suspense ?fallback:_ children =
  suspense_create_element suspense_t Js.null children

let null = Obj.magic Js.null

external text : string -> element = "%identity"

let textf fmt = Printf.ksprintf text fmt

type html_props

let html_props_null : html_props = Obj.magic Js.null

type unsafeHTML = { __html : string }

external html_props :
  ?key:string ->
  ?className:string ->
  ?children:children ->
  ?onClick:(unit -> unit) ->
  ?dangerouslySetInnerHTML:unsafeHTML ->
  unit ->
  html_props = ""
[@@mel.obj]

external unsafe_create_html_element :
  string -> html_props -> element array -> element = "createElement"
[@@mel.module "react"] [@@mel.variadic]

external use_state : (unit -> 'a) -> 'a * (('a -> 'a) -> unit)
  = "useState"
[@@mel.module "react"]

external use_effect' : (unit -> unit) -> 'a array -> unit = "useEffect"
[@@mel.module "react"]

external use_effect : (unit -> unit -> unit) -> 'a array -> unit
  = "useEffect"
[@@mel.module "react"]

external start_transition : (unit -> unit) -> unit = "startTransition"
[@@mel.module "react"]

type 'a promise = 'a Env_browser.Promise.t

external use : 'a promise -> 'a = "use" [@@mel.module "react"]

external render_to_string : element -> string = "renderToString"
[@@mel.module "react-dom/server"]
