open! Import

type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise
(** React.use raises this when the promise is not resolved yet *)

type unsafe_html = { __html : string }

module Html_prop = struct
  type prop = string * value
  and value = [ `String of string | `Bool of bool | `Int of int ]

  let s v : value = `String v
  let b v : value = `Bool v
  let className v = "className", s v
  let href v = "href", s v
  let _type v = "type", s v
  let checked v = "checked", b v
  let value v = "value", s v
end

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
      props : Html_prop.prop list;
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

and client_props =
  (string * [ `Json of string | `Element of element ]) list

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
let use_effect _thunk _deps = raise Browser_only
let use_effect' _thunk _deps = raise Browser_only
let start_transition _thunk = raise Browser_only

type 'a promise = 'a Lwt.t

let use _promise = raise Browser_only
