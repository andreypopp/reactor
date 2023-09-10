type element
type children = element array
type dom_element = Dom.element
type 'a nullable = 'a Js.nullable
type 'props component = 'props Js.t -> element constraint 'props = < .. >

external unsafe_create_element :
  < children : children ; .. > component ->
  'props Js.t ->
  element array ->
  element = "createElement"
[@@mel.module "react"] [@@mel.variadic]

external unsafe_create_element' : 'props component -> 'props -> element
  = "createElement"
[@@mel.module "react"]

type suspense

type suspense_props =
  < children : element array
  ; fallback : element array option
  ; key : string option >

external suspense : suspense_props component = "Suspense"
[@@mel.module "react"]

external suspense__props :
  ?key:string ->
  ?fallback:children ->
  children:children ->
  unit ->
  suspense_props Js.t = ""
[@@mel.obj]

external array : element array -> element = "%identity"

let list xs = array (Array.of_list xs)
let null = Obj.magic Js.null

external text : string -> element = "%identity"

let textf fmt = Printf.ksprintf text fmt

type html_props = React_browser_html_props.props

let html_props_null : html_props = Obj.magic Js.null
let html_props = React_browser_html_props.props

external unsafe_create_html_element :
  string -> html_props -> element array -> element = "createElement"
[@@mel.module "react"] [@@mel.variadic]

external use_state : (unit -> 'a) -> 'a * (('a -> 'a) -> unit)
  = "useState"
[@@mel.module "react"]

type dep

external to_dep : 'a -> dep = "%identity"

external use_effect_ : (unit -> unit -> unit) -> dep array -> unit
  = "useEffect"
[@@mel.module "react"]

external use_effect_' : (unit -> unit) -> dep array -> unit = "useEffect"
[@@mel.module "react"]

let use_effect0 f = use_effect_ f [||]
let use_effect1 a f = use_effect_ f [| to_dep a |]
let use_effect2 a b f = use_effect_ f [| to_dep a; to_dep b |]
let use_effect0' f = use_effect_' f [||]
let use_effect1' a f = use_effect_' f [| to_dep a |]
let use_effect2' a b f = use_effect_' f [| to_dep a; to_dep b |]

external use_layout_effect_ : (unit -> unit -> unit) -> dep array -> unit
  = "useLayoutEffect"
[@@mel.module "react"]

external use_layout_effect_' : (unit -> unit) -> dep array -> unit
  = "useLayoutEffect"
[@@mel.module "react"]

let use_layout_effect0 f = use_layout_effect_ f [||]
let use_layout_effect1 a f = use_layout_effect_ f [| to_dep a |]

let use_layout_effect2 a b f =
  use_layout_effect_ f [| to_dep a; to_dep b |]

let use_layout_effect0' f = use_layout_effect_' f [||]
let use_layout_effect1' a f = use_layout_effect_' f [| to_dep a |]

let use_layout_effect2' a b f =
  use_layout_effect_' f [| to_dep a; to_dep b |]

external use_memo_ : (unit -> 'a) -> dep array -> 'a = "useMemo"
[@@mel.module "react"]

let use_memo0 f = use_memo_ f [||]
let use_memo1 a f = use_memo_ f [| to_dep a |]
let use_memo2 a b f = use_memo_ f [| to_dep a; to_dep b |]
let use_callback0 (f : 'a -> 'b) = use_memo0 (fun () -> f)
let use_callback1 a (f : 'a -> 'b) = use_memo1 a (fun () -> f)
let use_callback2 a b (f : 'a -> 'b) = use_memo2 a b (fun () -> f)

type 'a ref = { mutable current : 'a Js.null_undefined }

external use_ref' : unit -> 'a ref = "useRef" [@@mel.module "react"]

let deref ref = Js.Null_undefined.toOption ref.current

let use_ref () =
  let ref = use_ref' () in
  let set =
    use_callback0 (fun v -> ref.current <- Js.Null_undefined.fromOption v)
  in
  ref, set

let use_dom_ref () =
  let ref = use_ref' () in
  let set = use_callback0 (fun v -> ref.current <- v) in
  ref, set

external start_transition : (unit -> unit) -> unit = "startTransition"
[@@mel.module "react"]

type 'a promise = 'a Env.Promise.t

external use : 'a promise -> 'a = "use" [@@mel.module "react"]
