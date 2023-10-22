type json = Yojson.Basic.t
type ('a, 'b) query_endpoint
type ('a, 'b) mutation_endpoint

val define_query :
  yojson_of_input:('a -> json) ->
  output_of_yojson:(json -> 'b) ->
  path:string ->
  ('a, 'b) query_endpoint

val define_mutation :
  yojson_of_input:('a -> json) ->
  output_of_yojson:(json -> 'b) ->
  path:string ->
  ('a, 'b) mutation_endpoint

type 'b query
type 'b mutation

val make_query : ('a, 'b) query_endpoint -> 'a -> 'b query
val make_mutation : ('a, 'b) mutation_endpoint -> 'a -> 'b mutation
val run_query : 'a query -> 'a Promise.t
val run_mutation : 'a mutation -> 'a Promise.t
val invalidate : 'a query -> unit
