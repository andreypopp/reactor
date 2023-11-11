module Codec : sig
  type ctx

  type 'a t = {
    columns : string -> (string * string) list;
    decode : 'a decode;
    bind : 'a bind;
  }

  and 'a decode = Sqlite3.Data.t array -> ctx -> 'a
  and 'a bind = 'a -> ctx -> Sqlite3.stmt -> unit
end

type 's opt

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
  val col : string -> string -> 'a Codec.decode -> 'a expr
  val col_opt : string -> string -> 'a Codec.decode -> 'a expr_nullable
end

type 'a e = 'a E.expr
type 'a expr_nullable = 'a E.expr_nullable
type any_expr = Any_expr : ('a, 'n) E.t -> any_expr
type fields = (any_expr * string) list
type 's make_scope = string -> 's

type 's meta = {
  scope : string * string -> 's;
  fields : string -> (any_expr * string) list;
}

type ('a, 's) table = {
  table : string;
  codec : 'a Codec.t;
  columns : (string * string) list;
  scope : 's make_scope;
  fields : fields;
}

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

val create : ('a, _) table -> db -> unit
val insert : ('a, _) table -> db -> 'a -> unit

val fold_table :
  ('a, _) table -> db -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

val iter_table : ('a, _) table -> db -> f:('a -> unit) -> unit

module Q : sig
  type order

  val asc : (_, _) E.t -> order
  val desc : (_, _) E.t -> order

  type ('a, 's) t

  val from : ('a, 'b) table -> ('b, 'a) t
  val where : ('a -> bool e) -> ('a, 'b) t -> ('a, 'b) t
  val order_by : ('a -> order list) -> ('a, 'b) t -> ('a, 'b) t

  val left_join :
    ('a, 'b) t ->
    ('c * 'a -> bool e) ->
    ('c, 'd) t ->
    ('c * 'a opt, 'd * 'b option) t

  val select :
    ('scope -> 'next_scope make_scope * fields * 'value Codec.decode) ->
    ('scope, _) t ->
    ('next_scope, 'value) t
end

type ('s, 'a) q = ('s, 'a) Q.t

val fold_query : db -> ('a, 'b) q -> init:'c -> f:('c -> 'b -> 'c) -> 'c
val iter_query : db -> ('a, 'b) q -> f:('b -> unit) -> unit

module P : sig
  type 'a t

  val get : ?name:string -> 'a e -> 'a t
  val get_opt : ?name:string -> 'a expr_nullable -> 'a option t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val decode : 'a t -> 'a Codec.decode
  val fields : 'a t -> (any_expr * string) list
  val select : ('a -> 'b t) -> ('a, 'c) q -> (unit, 'b) q

  val select' :
    ('a -> 'b make_scope) -> ('a -> 'c t) -> ('a, 'd) q -> ('b, 'c) q

  val fold :
    ('scope, 'c) q ->
    ('scope -> 'b t) ->
    db ->
    init:'d ->
    f:('d -> 'b -> 'd) ->
    'd

  val iter : ('a, 'c) q -> ('a -> 'b t) -> db -> f:('b -> unit) -> unit
end

module Builtins : sig
  val bool_codec : bool Codec.t
  val string_codec : string Codec.t
  val int_codec : int Codec.t
  val float_codec : float Codec.t
  val option_codec : 'a Codec.t -> 'a option Codec.t

  type string_scope = string e
  type int_scope = int e
  type float_scope = float e
  type bool_scope = bool e

  val string_meta : string e meta
  val int_meta : int e meta
  val float_meta : float e meta
  val bool_meta : bool e meta
end
