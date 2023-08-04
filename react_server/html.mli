type t
type attrs = (string * [ `String of string | `Bool of bool ]) list

val node : string -> attrs -> t list option -> t
val text : string -> t
val raw : string -> t
val splice : ?sep:string -> t list -> t
val to_string : t -> string
