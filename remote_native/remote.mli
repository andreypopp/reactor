type json = Yojson.Safe.t

type ('a, 'b) query_def
(** Query definition. *)

val define_query :
  yojson_of_input:('a -> json) ->
  yojson_of_output:('b -> json) ->
  path:string ->
  ('a -> 'b Promise.t) ->
  ('a, 'b) query_def

type 'b query
(** Query. *)

val make_query : ('a, 'b) query_def -> 'a -> 'b query

val run : 'b query -> 'b Promise.t
(** Run query and wait till result is available. *)

module Runner : sig
  type ctx

  val create : unit -> ctx

  type running =
    | Running : {
        path : string;
        input : json;
        yojson_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running

  val with_ctx : ctx -> (unit -> 'a) -> 'a * running list
end
