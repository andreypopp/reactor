open! Import

type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise
(** React.use raises this when the promise is not resolved yet *)

type unsafe_html = { __html : string }

module Html_props = React_server_html_props

let current_context : Hmap.t Lwt.key = Lwt.new_key ()

let with_context context f =
  Lwt.with_value current_context (Some context) f

type 'a context = { key : 'a Hmap.key; default : 'a }

type element =
  | El_null : element
  | El_frag : children -> element
  | El_context : 'a context * 'a * element -> element
  | El_suspense : {
      children : element;
      fallback : element;
      key : string option;
    }
      -> element
  | El_text : string -> element
  | El_html : {
      tag_name : string;
      key : string option;
      props : Html_props.props;
      children : html_children option;
    }
      -> element
  | El_thunk : (unit -> element) -> element
  | El_async_thunk : (unit -> element Lwt.t) -> element
  | El_client_thunk : {
      import_module : string;
      import_name : string;
      props : client_props;
      thunk : element;
    }
      -> element

and children = element array

and html_children =
  | Html_children of element
  | Html_children_raw of unsafe_html

and client_props = (string * client_prop) list

and client_prop =
  | Json : json -> client_prop
  | Element : element -> client_prop
  | Promise : 'a Promise.t * ('a -> json) -> client_prop

let null = El_null
let array els = El_frag els
let list els = array (Array.of_list els)
let string s = El_text s
let int s = El_text (string_of_int s)
let float s = El_text (string_of_float s)
let stringf fmt = ksprintf string fmt
let thunk f = El_thunk f
let async_thunk f = El_async_thunk f

module Suspense = struct
  let make ?key ?(fallback = null) ~children () =
    El_suspense { children; fallback; key }
end

let client_thunk ?(import_name = "") import_module props thunk =
  El_client_thunk { import_module; import_name; props; thunk }

let unsafe_create_html_element ?key tag_name props children =
  El_html { tag_name; key; props; children }

exception Browser_only

let useState init = init (), fun _update -> raise Browser_only
let useEffect _thunk = raise Browser_only
let useEffect1 _thunk _deps = raise Browser_only
let use_memo0 f = f ()
let use_memo1 _a f = f ()
let use_memo2 _a _b f = f ()
let use_callback0 f = f
let use_callback1 _a f = f
let use_callback2 _a _b f = f

module Context = struct
  type 'a t = 'a context

  let provider ctx ?key:_ ~value ~children () =
    El_context (ctx, value, children)
end

let useContext ctx =
  match Lwt.get current_context with
  | None -> ctx.default
  | Some ctx_map -> (
      match Hmap.find ctx.key ctx_map with
      | None -> ctx.default
      | Some v -> v)

let createContext default = { default; key = Hmap.Key.create () }

type dom_element
type 'a nullable
type 'a ref = { mutable current : 'a }

let useRef value = { current = value }
let startTransition _thunk = raise Browser_only

type 'a promise = 'a Lwt.t

let use _promise = raise Browser_only

let useOptimistic state _f =
  let update _ = raise Browser_only in
  state, update
