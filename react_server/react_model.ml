open! Import

type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise
(** React.use raises this when the promise is not resolved yet *)

type unsafe_html = { __html : string }

module Html_props = React_server_html_props

type element =
  | El_null : element
  | El_suspense : {
      children : children;
      fallback : children;
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
  | Html_children of children
  | Html_children_raw of unsafe_html

and client_props = (string * client_prop) list

and client_prop =
  | Json : string -> client_prop
  | Element : element -> client_prop
  | Promise : 'a Env.Promise.t * ('a -> json) -> client_prop

let null = El_null
let text s = El_text s
let textf fmt = ksprintf text fmt
let thunk f = El_thunk f
let async_thunk f = El_async_thunk f

let suspense ?key ?(fallback = [| null |]) children =
  El_suspense { children; fallback; key }

let client_thunk ?(import_name = "") import_module props thunk =
  El_client_thunk { import_module; import_name; props; thunk }

let unsafe_create_html_element ?key tag_name props children =
  El_html { tag_name; key; props; children }

exception Browser_only

let use_state init = init (), fun _update -> raise Browser_only
let use_effect0 _thunk = raise Browser_only
let use_effect1 _a _thunk = raise Browser_only
let use_effect2 _a _b _thunk = raise Browser_only
let use_effect0' _thunk = raise Browser_only
let use_effect1' _a _thunk = raise Browser_only
let use_effect2' _a _b _thunk = raise Browser_only
let use_layout_effect0 _thunk = raise Browser_only
let use_layout_effect1 _a _thunk = raise Browser_only
let use_layout_effect2 _a _b _thunk = raise Browser_only
let use_layout_effect0' _thunk = raise Browser_only
let use_layout_effect1' _a _thunk = raise Browser_only
let use_layout_effect2' _a _b _thunk = raise Browser_only
let use_memo0 f = f ()
let use_memo1 _a f = f ()
let use_memo2 _a _b f = f ()
let use_callback0 f = f
let use_callback1 _a f = f
let use_callback2 _a _b f = f

type dom_element
type 'a nullable
type nonrec 'a ref = 'a option ref

let use_ref () =
  let ref = ref None in
  let set v = ref := v in
  ref, set

let use_dom_ref () =
  let ref = ref None in
  let set _ = raise Browser_only in
  ref, set

let deref ref = ref.contents
let start_transition _thunk = raise Browser_only

type 'a promise = 'a Lwt.t

let use _promise = raise Browser_only
