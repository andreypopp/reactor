type json = Yojson.Safe.t

type 'a req
(** A request to obtain value of type ['a]. *)

val make : output_of_yojson:(json -> 'a) -> string -> json -> 'a req
(** [make ~output_of_yojson path params] makes a new request. *)

val run : 'a req -> 'a Promise.t
(** [run req] runs request. *)

val invalidate : 'a req -> unit
(** [invalidate req] invalidates request cache] *)
