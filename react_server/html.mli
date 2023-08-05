type t
type attrs = (string * [ `String of string | `Bool of bool ]) list

val node : string -> attrs -> t list option -> t
val text : string -> t
val raw : string -> t
val rawf : ('a, unit, string, t) format4 -> 'a
val splice : ?sep:string -> t list -> t
val s : string -> [ `String of string | `Bool of bool ]
val b : bool -> [ `String of string | `Bool of bool ]
val to_string : t -> string
