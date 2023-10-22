type json = Yojson.Basic.t
type ('a, 'b) query_endpoint
type ('a, 'b) mutation_endpoint

val define_query :
  yojson_of_input:('a -> json) ->
  yojson_of_output:('b -> json) ->
  path:string ->
  ('a -> 'b Promise.t) ->
  ('a, 'b) query_endpoint

val define_mutation :
  yojson_of_input:('a -> json) ->
  yojson_of_output:('b -> json) ->
  path:string ->
  ('a -> 'b Promise.t) ->
  ('a, 'b) mutation_endpoint

type 'b query
type 'b mutation

val make_query : ('a, 'b) query_endpoint -> 'a -> 'b query
val make_mutation : ('a, 'b) mutation_endpoint -> 'a -> 'b mutation
val run_query : 'b query -> 'b Promise.t
val run_mutation : 'b mutation -> 'b Promise.t

module Runner : sig
  type ctx

  val create : unit -> ctx
  val wait : ctx -> unit Lwt.t

  type running =
    | Running : {
        path : string;
        input : json;
        yojson_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running

  val with_ctx : ctx -> (unit -> 'a) -> 'a * running list

  val with_ctx_async :
    ctx -> (unit -> 'a Promise.t) -> ('a * running list) Promise.t
end
