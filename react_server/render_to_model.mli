open! Import

type model = json

val node :
  tag_name:string ->
  key:string option ->
  props:(string * model) list ->
  model option ->
  model

val ref : import_module:string -> import_name:string -> model
val null : model
val text : string -> model
val list : model list -> model
val suspense : key:string option -> model -> model
val suspense_placeholder : key:string option -> int -> model

type chunk = C_tree of model | C_ref of model

val chunk_to_string : int * chunk -> string

val render : React_model.element -> (string -> unit Lwt.t) -> unit Lwt.t
(** Render React elements into a serialized model. *)
