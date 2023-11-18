(** encode/decode database values into OCaml values *)
module Codec : sig
  type ctx

  type 'a t = {
    columns : string -> column list;
    decode : 'a decode;
    bind : 'a bind;
  }

  and column = { field : string option; column : string; type_ : string }
  and 'a decode = Sqlite3.Data.t array -> ctx -> 'a
  and 'a bind = 'a -> ctx -> Sqlite3.stmt -> unit
end

type 's opt
type 's agg

(** expression DSL *)
module E : sig
  type null = [ `not_null | `null ]
  type not_null = [ `not_null ]
  type 'a n = [< `not_null | `null ] as 'a
  type ('a, 'n) t
  type 'a expr = ('a, not_null) t
  type 'a expr_nullable = ('a, null) t

  val int : int -> int expr
  val bool : bool -> bool expr
  val string : string -> string expr
  val eq : ('a, _ n) t -> ('b, _ n) t -> (bool, _ n) t
  val and_ : (bool, _ n) t -> (bool, _ n) t -> (bool, _ n) t
  val or_ : (bool, _ n) t -> (bool, _ n) t -> (bool, _ n) t
  val ( = ) : ('a, _ n) t -> ('a, _ n) t -> (bool, _ n) t
  val ( && ) : (bool, _ n) t -> (bool, _ n) t -> (bool, _ n) t
  val ( || ) : (bool, _ n) t -> (bool, _ n) t -> (bool, _ n) t
  val coalesce : ('a, _) t -> ('a, 'n n) t -> ('a, 'n n) t
  val iif : (bool, _ n) t -> ('a, _ n) t -> ('a, _ n) t -> ('a, _ n) t
  val iif' : (bool, _ n) t -> ('a, _ n) t -> 'a expr_nullable
  val of_opt : 's opt -> ('s -> ('a, _) t) -> 'a expr_nullable
  val null : unit -> 'a expr_nullable
  val nullable : ('a, _) t -> 'a expr_nullable

  val as_col : string -> string -> ('a, 'n) t -> ('a, 'n) t
  (** FOR INTERNAL USE ONLY *)

  val col : string -> string -> 'a Codec.decode -> 'a expr
  (** FOR INTERNAL USE ONLY *)

  val col_opt : string -> string -> 'a Codec.decode -> 'a expr_nullable
  (** FOR INTERNAL USE ONLY *)
end

type 'a expr = 'a E.expr
type 'a expr_nullable = 'a E.expr_nullable
type any_expr = Any_expr : (_, _) E.t -> any_expr
type fields = (any_expr * string) list
type 's make_scope = string * string -> 's

type 's meta = {
  scope : string * string -> 's;
  fields : string -> (any_expr * string) list;
}

type db = Sqlite3.db

type ('row, 'scope, 'pk) table = {
  table : string;
  codec : 'row Codec.t;
  columns : Codec.column list;
  unique_columns : Codec.column list option;
  primary_key_columns : Codec.column list;
  primary_key_bind : 'pk Codec.bind;
  primary_key : 'row -> 'pk;
  scope : 'scope make_scope;
  fields : fields;
}

(** query DSL *)
module Q : sig
  type order

  val asc : (_, _) E.t -> order * any_expr
  val desc : (_, _) E.t -> order * any_expr

  type ('a, 's) t

  val from : ?n:string -> ('row, 'scope, _) table -> ('scope, 'row) t

  val where :
    ?n:string ->
    ('scope, 'row) t ->
    ('scope -> (bool, _) E.t) ->
    ('scope, 'row) t

  val order_by :
    ?n:string ->
    ('scope, 'row) t ->
    ('scope -> (order * any_expr) list) ->
    ('scope, 'row) t

  val left_join :
    ?na:string ->
    ?nb:string ->
    ('scope_a, 'row_a) t ->
    ('scope_b, 'row_b) t ->
    (< _1 : 'scope_a ; _2 : 'scope_b > -> bool expr) ->
    (< _1 : 'scope_a ; _2 : 'scope_b opt >, 'row_a * 'row_b option) t

  val select :
    ?n:string ->
    ('scope, _) t ->
    ('scope -> 'next_scope make_scope * fields * 'value Codec.decode) ->
    ('next_scope, 'value) t
  (** FOR INTERNAL USE ONLY *)

  val group_by :
    ?n:string ->
    ('scope, _) t ->
    ('scope -> 'key * any_expr list) ->
    ('key * 'scope agg ->
    'next_scope make_scope * fields * 'value Codec.decode) ->
    ('next_scope, 'value) t
  (** FOR INTERNAL USE ONLY *)

  val print_sql : (_, _) t -> unit
end

val init :
  ?mode:[ `NO_CREATE | `READONLY ] ->
  ?uri:bool ->
  ?memory:bool ->
  ?mutex:[ `FULL | `NO ] ->
  ?cache:[ `PRIVATE | `SHARED ] ->
  ?vfs:string ->
  string ->
  db
(** initialize database *)

val create : ('row, _, _) table -> db -> unit
(** create table *)

val insert : ('row, _, _) table -> db -> 'row -> unit
(** insert new row into a table *)

val upsert : ('row, _, _) table -> db -> 'row -> unit
(** upsert (try insert but fallback to update if row already exists) a row into a table *)

val update : ('row, _, _) table -> db -> 'row -> unit
(** update a row in a table *)

val delete : (_, _, 'pk) table -> db -> 'pk -> unit
(** delete a row by pk *)

val delete_where :
  (_, 'scope, _) table -> where:('scope -> (bool, _) E.t) -> db -> unit
(** delete mutiple rows by condition *)

val make_query_with :
  sql:Containers_pp.t ->
  (ctx:Codec.ctx -> stmt:Sqlite3.stmt -> (unit -> unit) -> 'a) ->
  db ->
  'a

val transaction : db -> (unit -> 'a) -> 'a

type ('s, 'a) q = ('s, 'a) Q.t

val iter_query : db -> (_, 'row) q -> f:('row -> unit) -> unit
(** iterate over query results *)

val fold_query :
  db -> (_, 'row) q -> init:'acc -> f:('acc -> 'row -> 'acc) -> 'acc
(** fold over query results *)

val iter_table : ('row, _, _) table -> db -> f:('row -> unit) -> unit
(** iterate over all values of a table *)

val fold_table :
  ('row, _, _) table ->
  db ->
  init:'acc ->
  f:('acc -> 'row -> 'acc) ->
  'acc
(** fold over all values of a table *)

module Primitives : sig
  val bool_codec : bool Codec.t
  val string_codec : string Codec.t
  val int_codec : int Codec.t
  val float_codec : float Codec.t
  val option_codec : 'a Codec.t -> 'a option Codec.t

  type string_scope = string expr
  type int_scope = int expr
  type float_scope = float expr
  type bool_scope = bool expr

  val string_meta : string expr meta
  val int_meta : int expr meta
  val float_meta : float expr meta
  val bool_meta : bool expr meta
  val option_bind : 'a Codec.bind -> 'a option Codec.bind
end

(** decode query results *)
module P : sig
  type 'a t

  val get : ?name:string -> 'a expr -> 'a t
  val get_opt : ?name:string -> 'a expr_nullable -> 'a option t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val decode : 'a t -> 'a Codec.decode

  val select :
    ?n:string ->
    ('scope, 'row) q ->
    ('scope -> 'next_row t) ->
    (unit, 'next_row) q

  val fold :
    ('scope, 'c) q ->
    ('scope -> 'b t) ->
    db ->
    init:'d ->
    f:('d -> 'b -> 'd) ->
    'd

  val iter : ('a, 'c) q -> ('a -> 'b t) -> db -> f:('b -> unit) -> unit

  val select' :
    ?n:string ->
    ('scope, 'row) q ->
    ('scope -> 'next_scope make_scope) ->
    ('scope -> 'next_row t) ->
    ('next_scope, 'next_row) q
  (** FOR INTERNAL USE ONLY *)
end

module Sql : sig
  val insert_sql : ('a, 'b, 'c) table -> Containers_pp.t
  val update_sql : ('a, 'b, 'c) table -> Containers_pp.t
  val upsert_sql : ('a, 'b, 'c) table -> Containers_pp.t
  val delete_sql : ('a, 'b, 'c) table -> Containers_pp.t
end
