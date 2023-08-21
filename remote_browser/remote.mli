type json = Yojson.Safe.t

type ('a, 'b) query_def
(** Query definition. *)

val define_query :
  yojson_of_input:('a -> json) ->
  output_of_yojson:(json -> 'b) ->
  path:string ->
  ('a, 'b) query_def

type 'b query
(** A request to obtain value of type ['a]. *)

val make_query : ('a, 'b) query_def -> 'a -> 'b query

val run : 'a query -> 'a Promise.t
(** [run query] runs request. *)

val invalidate : 'a query -> unit
(** [invalidate query] invalidates request cache] *)
