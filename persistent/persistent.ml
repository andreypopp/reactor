open Printf
open ContainersLabels

module Codec = struct
  type ctx = { mutable idx : int }

  type 'a t = {
    columns : string -> (string * string) list;
    decode : 'a decode;
    bind : 'a bind;
  }

  and 'a decode = Sqlite3.Data.t array -> ctx -> 'a
  and 'a bind = 'a -> ctx -> Sqlite3.stmt -> unit

  module Builtins = struct
    let option_decode decode row ctx =
      let v =
        (* TODO: this is wrong, this way decoder can still can succeed *)
        match row.(ctx.idx) with
        | Sqlite3.Data.NULL -> None
        | _ -> Some (decode row ctx)
      in
      ctx.idx <- ctx.idx + 1;
      v

    let option_codec codec =
      let bind v ctx stmt =
        match v with
        | Some v -> codec.bind v ctx stmt
        | None ->
            let v = Sqlite3.Data.NULL in
            Sqlite3.Rc.check (Sqlite3.bind stmt ctx.idx v);
            ctx.idx <- ctx.idx + 1
      in
      {
        decode = option_decode codec.decode;
        columns = codec.columns;
        bind;
      }

    let bool_codec =
      let columns name = [ name, "INT" ] in
      let decode row ctx =
        let v =
          match row.(ctx.idx) with
          | Sqlite3.Data.INT 0L -> false
          | Sqlite3.Data.INT 1L -> true
          | _ -> failwith "sql type error: expected bool (int)"
        in
        ctx.idx <- ctx.idx + 1;
        v
      in
      let bind v ctx stmt =
        Sqlite3.Rc.check
          (Sqlite3.bind stmt ctx.idx (INT (Int64.of_int (Bool.to_int v))));
        ctx.idx <- ctx.idx + 1
      in
      { columns; decode; bind }

    let string_codec =
      let columns name = [ name, "TEXT" ] in
      let decode row ctx =
        let v =
          match row.(ctx.idx) with
          | Sqlite3.Data.TEXT v -> v
          | _ -> failwith "sql type error: expected text"
        in
        ctx.idx <- ctx.idx + 1;
        v
      in
      let bind v ctx stmt =
        Sqlite3.Rc.check (Sqlite3.bind stmt ctx.idx (TEXT v));
        ctx.idx <- ctx.idx + 1
      in
      { columns; decode; bind }

    let int_codec =
      let columns name = [ name, "INT" ] in
      let decode row ctx =
        let v =
          match row.(ctx.idx) with
          | Sqlite3.Data.INT v -> Int64.to_int v
          | _ -> failwith "sql type error: expected int"
        in
        ctx.idx <- ctx.idx + 1;
        v
      in
      let bind v ctx stmt =
        Sqlite3.Rc.check
          (Sqlite3.bind stmt ctx.idx (INT (Int64.of_int v)));
        ctx.idx <- ctx.idx + 1
      in
      { columns; decode; bind }

    let float_codec =
      let columns name = [ name, "FLOAT" ] in
      let decode row ctx =
        let v =
          match row.(ctx.idx) with
          | Sqlite3.Data.FLOAT v -> v
          | _ -> failwith "sql type error: expected float"
        in
        ctx.idx <- ctx.idx + 1;
        v
      in
      let bind v ctx stmt =
        Sqlite3.Rc.check (Sqlite3.bind stmt ctx.idx (FLOAT v));
        ctx.idx <- ctx.idx + 1
      in
      { columns; decode; bind }
  end
end

type 's opt = Opt of 's

module E = struct
  open Codec.Builtins

  type not_null = private NOT_NULL
  type null = private NULL

  type ('a, 'n) t = {
    sql : string;
    decode : 'a Codec.decode;
    cols : (string * string) list;
  }

  let cols e = e.cols

  type 'a expr = ('a, not_null) t
  type 'a expr_nullable = ('a, null) t

  let int v =
    { sql = string_of_int v; decode = int_codec.decode; cols = [] }

  let bool v =
    { sql = string_of_bool v; decode = bool_codec.decode; cols = [] }

  let string v =
    {
      sql = Printf.sprintf "'%s'" v;
      decode = string_codec.decode;
      cols = [];
    }

  let make_binop decode op a b =
    {
      sql = Printf.sprintf "(%s %s %s)" a.sql op b.sql;
      decode;
      cols = a.cols @ b.cols;
    }

  let eq a b = make_binop bool_codec.decode "=" a b
  let and_ a b = make_binop bool_codec.decode "AND" a b
  let or_ a b = make_binop bool_codec.decode "OR" a b
  let ( = ) = eq
  let ( && ) = and_
  let ( || ) = or_

  let coalesce ov v =
    {
      sql = Printf.sprintf "COALESCE(%s, %s)" ov.sql v.sql;
      decode = v.decode;
      cols = ov.cols @ v.cols;
    }

  let col t col decode =
    { sql = sprintf "%s.%s" t col; decode; cols = [ t, col ] }

  let col_opt t col decode =
    { sql = sprintf "%s.%s" t col; decode; cols = [ t, col ] }

  let as_col t col e =
    { e with sql = sprintf "%s.%s" t col; cols = [ t, col ] }

  let to_sql e = e.sql
  let decode e = e.decode
  let decode_opt e = option_decode e.decode

  let of_opt (Opt s : 's opt) f =
    let e = f s in
    { sql = e.sql; decode = e.decode; cols = e.cols }
end

type 'a e = 'a E.expr
type 'a expr_nullable = 'a E.expr_nullable
type any_expr = Any_expr : ('a, 'n) E.t -> any_expr
type fields = (any_expr * string) list
type 's make_scope = string -> 's

type 's meta = {
  scope : string * string -> 's;
  fields : string -> fields;
}

type ('a, 's) table = {
  table : string;
  codec : 'a Codec.t;
  columns : (string * string) list;
  scope : 's make_scope;
  fields : fields;
}

type db = Sqlite3.db

let init = Sqlite3.db_open

let create t =
  let sql =
    Printf.sprintf "CREATE TABLE IF NOT EXISTS %s(%s)" t.table
      (t.columns
      |> List.map ~f:(fun (n, t) -> Printf.sprintf "%s %s" n t)
      |> String.concat ~sep:",")
  in
  fun db -> Sqlite3.Rc.check (Sqlite3.exec db sql)

let rec insert_work stmt =
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | OK -> insert_work stmt
  | rc -> Sqlite3.Rc.check rc

let insert t =
  let sql =
    Printf.sprintf "INSERT INTO %s(%s) VALUES(%s)" t.table
      (t.columns |> List.map ~f:fst |> String.concat ~sep:", ")
      (t.columns |> List.map ~f:(fun _ -> "?") |> String.concat ~sep:", ")
  in
  fun db ->
    let stmt = Sqlite3.prepare db sql in
    fun v ->
      let ctx = { Codec.idx = 1 } in
      Sqlite3.Rc.check (Sqlite3.reset stmt);
      t.codec.bind v ctx stmt;
      insert_work stmt

let fold_raw sql decode db ~init ~f =
  let stmt = Sqlite3.prepare db sql in
  let rc, acc =
    Sqlite3.fold stmt ~init ~f:(fun acc row ->
        let ctx = { Codec.idx = 0 } in
        let user = decode row ctx in
        f acc user)
  in
  Sqlite3.Rc.check rc;
  acc

let fold_table t db ~init ~f =
  let sql =
    Printf.sprintf "SELECT %s FROM %s"
      (t.columns |> List.map ~f:fst |> String.concat ~sep:", ")
      t.table
  in
  fold_raw sql t.codec.decode db ~f ~init

let iter_table t db ~f = fold_table ~init:() ~f:(fun () -> f) t db

module Q = struct
  type order = Asc : (_, _) E.t -> order | Desc : (_, _) E.t -> order

  let asc e = Asc e
  let desc e = Desc e

  type (_, _) t =
    | From : ('a, 's) table -> ('s, 'a) t
    | Where : ('s, 'a) t * ('s -> bool e) -> ('s, 'a) t
    | Order_by : ('s, 'a) t * ('s -> order list) -> ('s, 'a) t
    (* | Join : *)
    (*     ('sa, 'a) q * ('sb, 'b) q * ('sa * 'sb -> bool e) *)
    (*     -> ('sa * 'sb, 'a * 'b) q *)
    | Left_join :
        ('sa, 'a) t * ('sb, 'b) t * ('sa * 'sb -> bool e)
        -> ('sa * 'sb opt, 'a * 'b option) t
    | Select :
        ('s, 'a) t * ('s -> 's1 make_scope * fields * 'a1 Codec.decode)
        -> ('s1, 'a1) t

  type ('s, 'a) rel = {
    tree : tree;
    scope : 's make_scope;
    fields : (any_expr * string) list;
    mutable select : (any_expr * string) list;
    default_select : string;
    decode : 'a Codec.decode;
  }

  and tree =
    | SUBQUERY : (_, _) rel -> tree
    | FROM : _ table -> tree
    | WHERE : (_, _) rel * (_, _) E.t -> tree
    | ORDER_BY : (_, _) rel * order list -> tree
    | JOIN : (_, _) rel * (_, _) rel * (_, _) E.t -> tree

  module Containers_pp_aux = struct
    open Containers_pp

    let comma = text "," ^ newline
  end

  let rec print_rel : type s a. (s, a) rel -> Containers_pp.t =
   fun rel ->
    let open Containers_pp in
    let open Containers_pp_aux in
    let select =
      match rel.select with
      | [] -> text rel.default_select
      | select ->
          of_list ~sep:comma
            (fun (Any_expr e, n) -> textf "%s AS %s" (E.to_sql e) n)
            select
    in
    group (text "SELECT" ^ nest 2 (newline ^ select))
    ^/ print_tree rel.tree

  and print_tree : tree -> Containers_pp.t =
    let open Containers_pp in
    let open Containers_pp_aux in
    function
    | SUBQUERY rel -> print_from rel "t"
    | FROM t -> print_from' (text t.table) "t"
    | JOIN (a, b, e) ->
        print_from a "a"
        ^/ text "LEFT JOIN"
        ^+ bracket2 "(" (print_rel b) ") AS b"
        ^/ group (text "ON" ^ nest 2 (newline ^ text (E.to_sql e)))
    | WHERE (rel, e) ->
        print_from rel "t"
        ^/ group (text "WHERE" ^ nest 2 (newline ^ text (E.to_sql e)))
    | ORDER_BY (rel, os) ->
        print_from rel "t"
        ^/ group
             (text "ORDER BY"
             ^ nest 2 (newline ^ of_list ~sep:comma print_order os))

  and print_from : type s a. (s, a) rel -> string -> Containers_pp.t =
    let open Containers_pp in
    fun rel name -> print_from' (bracket2 "(" (print_rel rel) ")") name

  and print_from' rel name =
    let open Containers_pp in
    text "FROM" ^+ rel ^+ textf "AS %s" name

  and print_order =
    let open Containers_pp in
    function
    | Asc e -> textf "%s ASC" (E.to_sql e)
    | Desc e -> textf "%s DESC" (E.to_sql e)

  module Col_set = Set.Make (struct
    type t = string * string

    let compare = Ord.pair Ord.string Ord.string
  end)

  let used_expr e used =
    List.fold_left (E.cols e)
      ~f:(fun used col -> Col_set.add col used)
      ~init:used

  let used_fields fs used =
    List.fold_left fs ~init:used ~f:(fun used (Any_expr e, _) ->
        used_expr e used)

  let mark t rel used_select used =
    if not (Col_set.is_empty used_select) then
      rel.select <-
        List.filter rel.fields ~f:(fun (_, n) -> Col_set.mem (t, n) used)

  let rec trim_select : type s a. (s, a) rel -> unit =
   fun rel ->
    let used_select = used_fields rel.select Col_set.empty in
    match rel.tree with
    | SUBQUERY inner ->
        mark "t" inner used_select used_select;
        trim_select inner
    | FROM _ -> ()
    | WHERE (inner, e) ->
        let used = used_expr e used_select in
        mark "t" inner used_select used;
        trim_select inner
    | ORDER_BY (inner, fs) ->
        let used =
          List.fold_left fs
            ~f:(fun used -> function
              | Asc e -> used_expr e used | Desc e -> used_expr e used)
            ~init:used_select
        in
        mark "t" inner used_select used;
        trim_select inner
    | JOIN (a, b, e) ->
        let used = used_expr e used_select in
        mark "a" a used_select used;
        trim_select a;
        mark "b" b used_select used;
        trim_select b

  let forward_fields t fs =
    List.map fs ~f:(fun (Any_expr e, n) -> Any_expr (E.as_col t n e), n)

  let rec to_rel : type s a. (s, a) t -> (s, a) rel = function
    | From t ->
        {
          decode = t.codec.decode;
          scope = t.scope;
          tree = FROM t;
          fields = t.fields;
          select = [];
          default_select = "*";
        }
    | Where (q, e) ->
        let inner = to_rel q in
        let e = e (inner.scope "t") in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = WHERE (inner, e);
          fields = forward_fields "t" inner.fields;
          select = [];
          default_select = "t.*";
        }
    | Order_by (q, e) ->
        let inner = to_rel q in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = ORDER_BY (inner, e (inner.scope "t"));
          fields = forward_fields "t" inner.fields;
          select = [];
          default_select = "t.*";
        }
    (* | Join (a, b, e) -> *)
    (*     let a = to_rel a in *)
    (*     let b = to_rel b in *)
    (*     let scope = a.scope, b.scope in *)
    (*     let decode v = *)
    (*       let a = a.decode v in *)
    (*       let b = b.decode v in *)
    (*       a, b *)
    (*     in *)
    (*     { *)
    (*       decode; *)
    (*       scope; *)
    (*       sql = *)
    (*         Printf.sprintf "FROM (%s) AS a JOIN (%s) AS b ON %s" a.sql *)
    (*           b.sql *)
    (*           (E.to_rel (e scope)); *)
    (*       select = []; *)
    (*       default_select = "a.*, b.*"; *)
    (*     } *)
    | Left_join (a, b, e) ->
        let a = to_rel a in
        let b = to_rel b in
        let a_scope, b_scope = a.scope "a", b.scope "b" in
        let scope s = a.scope s, Opt (b.scope s) in
        let decode row ctx =
          let a = a.decode row ctx in
          let b = Codec.Builtins.option_decode b.decode row ctx in
          a, b
        in
        {
          decode;
          scope;
          tree = JOIN (a, b, e (a_scope, b_scope));
          select = [];
          fields =
            forward_fields "a" a.fields @ forward_fields "b" b.fields;
          default_select = "a.*, b.*";
        }
    | Select (q, f) ->
        let inner = to_rel q in
        let scope, fields, decode = f (inner.scope "t") in
        {
          decode;
          scope;
          tree = SUBQUERY inner;
          fields;
          select = fields;
          default_select = "t.*";
        }

  let to_sql q =
    let rel = to_rel q in
    trim_select rel;
    rel.decode, rel.scope, print_rel rel

  let from t = From t
  let where e q = Where (q, e)
  let order_by e q = Order_by (q, e)

  (* let join b e a = Join (a, b, e) *)
  let left_join b e a = Left_join (a, b, e)
  let select f q = Select (q, f)

  let fold db q ~init ~f =
    let decode, _scope, sql = to_sql q in
    let sql = Containers_pp.Pretty.to_string ~width:79 sql in
    print_endline sql;
    fold_raw sql decode db ~init ~f

  let iter db q ~f = fold db q ~init:() ~f:(fun () row -> f row)
end

type ('s, 'a) q = ('s, 'a) Q.t

let fold_query = Q.fold
let iter_query = Q.iter

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
end = struct
  type _ t =
    | E : 'a e * string option -> 'a t
    | EO : 'a expr_nullable * string option -> 'a option t
    | B : 'a t * 'b t -> ('a * 'b) t
    | F : 'a t * ('a -> 'b) -> 'b t

  let get ?name e = E (e, name)
  let get_opt ?name e = EO (e, name)
  let both a b = B (a, b)
  let map f a = F (a, f)
  let ( let+ ) a f = map f a
  let ( and+ ) a b = both a b

  let field_name name idx =
    match name with None -> sprintf "c%i" idx | Some name -> name

  let fields p =
    let rec fields : type a. int -> fields -> a t -> int * fields =
     fun idx fs p ->
      match p with
      | E (e, name) -> idx + 1, (Any_expr e, field_name name idx) :: fs
      | EO (e, name) -> idx + 1, (Any_expr e, field_name name idx) :: fs
      | B (a, b) ->
          let idx, fs = fields idx fs a in
          let idx, fs = fields idx fs b in
          idx, fs
      | F (a, _) -> fields idx fs a
    in
    let _idx, fs = fields 1 [] p in
    List.rev fs

  let rec decode : type a. a t -> a Codec.decode =
   fun p row ctx ->
    match p with
    | E (e, _) -> E.decode e row ctx
    | EO (e, _) -> E.decode_opt e row ctx
    | B (a, b) ->
        let a = decode a row ctx in
        let b = decode b row ctx in
        a, b
    | F (a, f) ->
        let a = decode a row ctx in
        f a

  let select' make_scope p q =
    Q.select
      (fun scope ->
        let p = p scope in
        let fields = fields p in
        make_scope scope, fields, decode p)
      q

  let select p q = select' (fun _ _ -> ()) p q

  let fold q p db ~init ~f =
    let q = select p q in
    let decode, _scope, sql = Q.to_sql q in
    let sql = Containers_pp.Pretty.to_string ~width:79 sql in
    print_endline sql;
    fold_raw sql decode db ~init ~f

  let iter q p db ~f = fold q p db ~init:() ~f:(fun () row -> f row)
end

module Builtins = struct
  include Codec.Builtins

  type string_scope = string e
  type int_scope = int e
  type float_scope = float e
  type bool_scope = bool e

  let string_meta =
    let scope (tbl, col) = E.col tbl col string_codec.decode in
    let fields n = [ Any_expr (E.col "t" n string_codec.decode), n ] in
    { scope; fields }

  let int_meta =
    let scope (tbl, col) = E.col tbl col int_codec.decode in
    let fields n = [ Any_expr (E.col "t" n int_codec.decode), n ] in
    { scope; fields }

  let float_meta =
    let fields n = [ Any_expr (E.col "t" n float_codec.decode), n ] in
    let scope (tbl, col) = E.col tbl col float_codec.decode in
    { scope; fields }

  let bool_meta =
    let fields n = [ Any_expr (E.col "t" n bool_codec.decode), n ] in
    let scope (tbl, col) = E.col tbl col bool_codec.decode in
    { scope; fields }
end
