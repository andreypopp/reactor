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

(** expression DSL *)
module E : sig
  type not_null = private NOT_NULL
  type null = private NULL
  type ('a, 'n) t
  type 'a expr = ('a, not_null) t
  type 'a expr_nullable = ('a, null) t

  val int : int -> int expr
  val bool : bool -> bool expr
  val string : string -> string expr
  val eq : ('a, 'n) t -> ('b, 'n) t -> (bool, 'n) t
  val and_ : (bool, 'n) t -> (bool, 'n) t -> (bool, 'n) t
  val or_ : (bool, 'n) t -> (bool, 'n) t -> (bool, 'n) t
  val ( = ) : ('a, 'n) t -> ('a, 'n) t -> (bool, 'n) t
  val ( && ) : (bool, 'n) t -> (bool, 'n) t -> (bool, 'n) t
  val ( || ) : (bool, 'n) t -> (bool, 'n) t -> (bool, 'n) t
  val coalesce : ('a, _) t -> ('a, 'n) t -> ('a, 'n) t
  val of_opt : 's opt -> ('s -> ('a, _) t) -> 'a expr_nullable

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

type ('row, 'scope, 'pk) table = {
  table : string;
  codec : 'row Codec.t;
  columns : Codec.column list;
  primary_key_columns : Codec.column list;
  primary_key_bind : 'pk Codec.bind;
  primary_key : 'row -> 'pk;
  scope : 'scope make_scope;
  fields : fields;
}

(** query DSL *)
module Q : sig
  type order

  val asc : (_, _) E.t -> order
  val desc : (_, _) E.t -> order

  type ('a, 's) t

  val from : ('row, 'scope, _) table -> ('scope, 'row) t

  val where :
    ('scope, 'row) t -> ('scope -> bool expr) -> ('scope, 'row) t

  val order_by :
    ('scope, 'row) t -> ('scope -> order list) -> ('scope, 'row) t

  val left_join :
    ('scope_a, 'row_a) t ->
    ('scope_b, 'row_b) t ->
    ('scope_a * 'scope_b -> bool expr) ->
    ('scope_a * 'scope_b opt, 'row_a * 'row_b option) t

  val select :
    ('scope, _) t ->
    ('scope -> 'next_scope make_scope * fields * 'value Codec.decode) ->
    ('next_scope, 'value) t
  (** FOR INTERNAL USE ONLY *)
end

type db = Sqlite3.db

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
    ('scope, 'row) q -> ('scope -> 'next_row t) -> (unit, 'next_row) q

  val fold :
    ('scope, 'c) q ->
    ('scope -> 'b t) ->
    db ->
    init:'d ->
    f:('d -> 'b -> 'd) ->
    'd

  val iter : ('a, 'c) q -> ('a -> 'b t) -> db -> f:('b -> unit) -> unit

  val select' :
    ('scope, 'row) q ->
    ('scope -> 'next_scope make_scope) ->
    ('scope -> 'next_row t) ->
    ('next_scope, 'next_row) q
  (** FOR INTERNAL USE ONLY *)
end

type void

val void_codec : void Codec.t
