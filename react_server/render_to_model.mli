open! Import

val node :
  tag_name:string ->
  key:string option ->
  props:(string * json) list ->
  json option ->
  json

val ref : import_module:string -> import_name:string -> json
val null : json
val text : string -> json
val list : json list -> json
val suspense : key:string option -> json -> json
val suspense_placeholder : key:string option -> int -> json
val promise_value : int -> json
val lazy_value : int -> json

type chunk = C_value of json | C_ref of json

val chunk_to_string : int * chunk -> string

val render : React_model.element -> (string -> unit Lwt.t) -> unit Lwt.t
(** Render React elements into a serialized model. *)
