open! Import

type model = json

val node :
  name:string -> props:(string * model) list -> model option -> model

val ref : import_module:string -> import_name:string -> model
val null : model
val text : string -> model
val list : model list -> model
val suspense : model -> model
val suspense_placeholder : int -> model

type chunk = C_tree of model | C_ref of model

val chunk_to_string : int * chunk -> string

val render : React.element -> (string -> unit Lwt.t) -> unit Lwt.t
(** Render React elements into a serialized model. *)
