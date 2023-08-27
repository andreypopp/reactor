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

let suspense ?key ?fallback:_ children =
  let props =
    match key with
    | None -> Js.Json.null
    | Some key -> (Obj.magic [%mel.obj { key }] : Js.Json.t)
  in
  suspense_create_element suspense_t props children

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

external use_effect' : (unit -> unit) -> 'a array -> unit = "useEffect"
[@@mel.module "react"]

external use_effect : (unit -> unit -> unit) -> 'a array -> unit
  = "useEffect"
[@@mel.module "react"]

external use_memo : (unit -> 'a) -> _ array -> 'a = "useMemo"
[@@mel.module "react"]

let use_callback (f : 'a -> 'b) deps = use_memo (fun () -> f) deps

external start_transition : (unit -> unit) -> unit = "startTransition"
[@@mel.module "react"]

type 'a promise = 'a Env.Promise.t

external use : 'a promise -> 'a = "use" [@@mel.module "react"]

external render_to_string : element -> string = "renderToString"
[@@mel.module "react-dom/server"]

external navigate : string -> unit = "React_of_caml_navigate"
