open! Import

type any_promise = Any_promise : 'a Lwt.t -> any_promise

exception Suspend of any_promise
(** React.use raises this when the promise is not resolved yet *)

type element =
  | El_null : element
  | El_suspense : { children : children; fallback : children } -> element
  | El_text : string -> element
  | El_html : string * html_props * children option -> element
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

and html_props =
  (string * [ `String of string | `Bool of bool | `Int of int ]) list

and client_props = (string * [ json | `Element of element ]) list

let null = El_null
let text s = El_text s
let textf fmt = ksprintf text fmt
let thunk f = El_thunk f
let async_thunk f = El_async_thunk f

let suspense ?(fallback = [| null |]) children =
  El_suspense { children; fallback }

let client_thunk ?(import_name = "") import_module props thunk =
  El_client_thunk { import_module; import_name; props; thunk }

type html_element = ?className:string -> children -> element

let html' tag_name : html_element =
 fun ?className children ->
  let props =
    [ "className", Option.map Html.s className ]
    |> List.filter_map ~f:(function
         | _, None -> None
         | n, Some v -> Some (n, v))
  in
  El_html (tag_name, props, Some children)

let h = html'
let html = html' "html"
let body = html' "body"
let head = html' "head"
let title = html' "title"
let div = html' "div"
let a = html' "a"
let span = html' "span"
let li = html' "li"
let ul = html' "ul"
let ol = html' "ol"
let h1 = html' "h1"
let h2 = html' "h2"
let h3 = html' "h3"
let use_effect _thunk _deps = assert false
let use_effect' _thunk _deps = assert false

type 'a promise = 'a Lwt.t

let use _promise = assert false
