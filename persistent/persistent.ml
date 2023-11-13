open Printf
open ContainersLabels

module Containers_pp = struct
  include Containers_pp

  let comma = text "," ^ newline
end

module Codec = struct
  type ctx = { mutable idx : int }

  type 'a t = {
    columns : string -> column list;
    decode : 'a decode;
    bind : 'a bind;
  }

  and column = { field : string option; column : string; type_ : string }
  and 'a decode = Sqlite3.Data.t array -> ctx -> 'a
  and 'a bind = 'a -> ctx -> Sqlite3.stmt -> unit

  module Primitives = struct
    let option_decode decode row ctx =
      let v =
        (* TODO: this is wrong, this way decoder can still can succeed *)
        match row.(ctx.idx) with
        | Sqlite3.Data.NULL -> None
        | _ -> Some (decode row ctx)
      in
      ctx.idx <- ctx.idx + 1;
      v

    let option_bind v_bind v ctx stmt =
      match v with
      | Some v -> v_bind v ctx stmt
      | None ->
          let v = Sqlite3.Data.NULL in
          Sqlite3.Rc.check (Sqlite3.bind stmt ctx.idx v);
          ctx.idx <- ctx.idx + 1

    let option_codec codec =
      {
        decode = option_decode codec.decode;
        columns = codec.columns;
        bind = option_bind codec.bind;
      }

    let bool_codec =
      let columns column =
        [ { field = None; column; type_ = "INTEGER NOT NULL" } ]
      in
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
      let columns column =
        [ { field = None; column; type_ = "TEXT NOT NULL" } ]
      in
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
      let columns column =
        [ { field = None; column; type_ = "INTEGER NOT NULL" } ]
      in
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
      let columns column =
        [ { field = None; column; type_ = "REAL NOT NULL" } ]
      in
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
  open Codec.Primitives

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

type 'a expr = 'a E.expr
type 'a expr_nullable = 'a E.expr_nullable
type any_expr = Any_expr : ('a, 'n) E.t -> any_expr
type fields = (any_expr * string) list
type 's make_scope = string * string -> 's

type 's meta = {
  scope : string * string -> 's;
  fields : string -> fields;
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

let init = Sqlite3.db_open

module Sql = struct
  let create_table_sql ~if_not_exists ~strict ~primary_key ~unique ~name
      ~columns =
    let open Containers_pp in
    let columns =
      List.map columns ~f:(fun col ->
          textf "%s %s" col.Codec.column col.type_)
    in
    let primary_key =
      match primary_key with
      | [] -> []
      | pk ->
          [
            text "PRIMARY KEY"
            ^+ bracket2 "("
                 (of_list ~sep:comma
                    (fun col -> text col.Codec.column)
                    pk)
                 ")";
          ]
    in
    let unique =
      match unique with
      | Some unique ->
          (List.map ~f:(fun cols ->
               text "UNIQUE"
               ^+ bracket2 "("
                    (of_list ~sep:comma
                       (fun col -> text col.Codec.column)
                       cols)
                    ")"))
            unique
      | None -> []
    in
    group
      (text "CREATE TABLE"
      ^ (if if_not_exists then text " IF NOT EXISTS" else nil)
      ^+ text name
      ^+ bracket2 "("
           (of_list ~sep:comma Fun.id (columns @ primary_key @ unique))
           ")"
      ^ (if strict then text " STRICT" else nil)
      ^ text ";")

  let where_primary_key_sql columns =
    let open Containers_pp in
    of_list
      ~sep:(newline ^ text "AND" ^ sp)
      (fun col -> textf "%s = ?" col.Codec.column)
      columns

  let insert_values_sql ~or_replace ~name ~columns =
    let open Containers_pp in
    group
      (text (if or_replace then "INSERT OR REPLACE" else "INSERT")
      ^+ textf "INTO %s" name
      ^+ bracket2 "("
           (of_list ~sep:comma (fun col -> text col.Codec.column) columns)
           ")"
      ^ group
          (newline
          ^ text "VALUES"
          ^+ bracket2 "("
               (of_list ~sep:comma (fun _ -> text "?") columns)
               ")"))

  let update_set_sql t =
    let open Containers_pp in
    text "SET"
    ^ nest 2
        (newline
        ^ of_list ~sep:comma
            (fun col -> textf "%s = ?" col.Codec.column)
            t.columns)

  let update_sql t =
    let open Containers_pp in
    group
      (textf "UPDATE %s" t.table
      ^+ update_set_sql t
      ^/ group
           (text "WHERE" ^+ where_primary_key_sql t.primary_key_columns))

  let insert_sql t =
    insert_values_sql ~or_replace:false ~name:t.table ~columns:t.columns

  let upsert_sql t =
    insert_values_sql ~or_replace:true ~name:t.table ~columns:t.columns

  let delete_sql t =
    let open Containers_pp in
    group
      (textf "DELETE FROM %s" t.table
      ^/ group
           (text "WHERE" ^+ where_primary_key_sql t.primary_key_columns))
end

let create t =
  let sql =
    Sql.create_table_sql ~if_not_exists:true ~strict:true
      ~primary_key:t.primary_key_columns
      ~unique:(Option.map List.return t.unique_columns)
      ~name:t.table ~columns:t.columns
  in
  let sql = Containers_pp.Pretty.to_string ~width:79 sql in
  print_endline sql;
  fun db -> Sqlite3.Rc.check (Sqlite3.exec db sql)

let rec step_stmt stmt =
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | OK -> step_stmt stmt
  | rc -> Sqlite3.Rc.check rc

let make_query_with ~sql f =
  let sql = Containers_pp.Pretty.to_string ~width:79 sql in
  print_endline sql;
  fun db ->
    let stmt = Sqlite3.prepare db sql in
    let ctx = { Codec.idx = 1 } in
    f ~ctx ~stmt (fun () ->
        Sqlite3.Rc.check (Sqlite3.reset stmt);
        step_stmt stmt)

let make_query ~sql ~bind =
  make_query_with ~sql (fun ~ctx ~stmt k v ->
      bind v ctx stmt;
      k ())

let insert t = make_query ~sql:(Sql.insert_sql t) ~bind:t.codec.bind
let upsert t = make_query ~sql:(Sql.upsert_sql t) ~bind:t.codec.bind
let delete t = make_query ~sql:(Sql.delete_sql t) ~bind:t.primary_key_bind

let update t =
  let bind v ctx stmt =
    t.codec.bind v ctx stmt;
    let pk = t.primary_key v in
    t.primary_key_bind pk ctx stmt
  in
  make_query ~sql:(Sql.update_sql t) ~bind

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
      (t.columns
      |> List.map ~f:(fun col -> col.Codec.column)
      |> String.concat ~sep:", ")
      t.table
  in
  fold_raw sql t.codec.decode db ~f ~init

let iter_table t db ~f = fold_table ~init:() ~f:(fun () -> f) t db

module Q = struct
  type order = Asc : (_, _) E.t -> order | Desc : (_, _) E.t -> order

  let asc e = Asc e
  let desc e = Desc e

  type (_, _) t =
    | From : ('a, 's, _) table -> ('s, 'a) t
    | Where : ('s, 'a) t * ('s -> bool expr) -> ('s, 'a) t
    | Order_by : ('s, 'a) t * ('s -> order list) -> ('s, 'a) t
    (* | Join : *)
    (*     ('sa, 'a) q * ('sb, 'b) q * ('sa * 'sb -> bool expr) *)
    (*     -> ('sa * 'sb, 'a * 'b) q *)
    | Left_join :
        ('sa, 'a) t * ('sb, 'b) t * ('sa * 'sb -> bool expr)
        -> ('sa * 'sb opt, 'a * 'b option) t
    | Select :
        ('s, 'a) t * ('s -> 's1 make_scope * fields * 'a1 Codec.decode)
        -> ('s1, 'a1) t

  type ('s, 'a) rel = {
    tree : tree;
    scope : 's make_scope;
    fields : (any_expr * string) list;
    mutable select : (any_expr * string) list;
    decode : 'a Codec.decode;
  }

  and tree =
    | SUBQUERY : (_, _) rel -> tree
    | FROM : _ table -> tree
    | WHERE : (_, _) rel * (_, _) E.t -> tree
    | ORDER_BY : (_, _) rel * order list -> tree
    | JOIN : (_, _) rel * (_, _) rel * (_, _) E.t -> tree

  let rec print_rel : type s a. (s, a) rel -> Containers_pp.t =
   fun rel ->
    let open Containers_pp in
    let select =
      let fields =
        match rel.select with [] -> rel.fields | select -> select
      in
      of_list ~sep:comma
        (fun (Any_expr e, n) -> textf "%s AS %s" (E.to_sql e) n)
        fields
    in
    group (text "SELECT" ^ nest 2 (newline ^ select))
    ^/ print_tree rel.tree

  and print_tree : tree -> Containers_pp.t =
    let open Containers_pp in
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

  let forward_fields ?prefix t fs =
    List.map fs ~f:(fun (Any_expr e, n) ->
        let n' =
          match prefix with
          | None -> n
          | Some prefix -> sprintf "%s_%s" prefix n
        in
        Any_expr (E.as_col t n e), n')

  let rec to_rel : type s a. (s, a) t -> (s, a) rel = function
    | From t ->
        {
          decode = t.codec.decode;
          scope = t.scope;
          tree = FROM t;
          fields = t.fields;
          select = [];
        }
    | Where (q, e) ->
        let inner = to_rel q in
        let e = e (inner.scope ("t", "")) in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = WHERE (inner, e);
          fields = forward_fields "t" inner.fields;
          select = [];
        }
    | Order_by (q, e) ->
        let inner = to_rel q in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = ORDER_BY (inner, e (inner.scope ("t", "")));
          fields = forward_fields "t" inner.fields;
          select = [];
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
        let inner_scope = a.scope ("a", ""), b.scope ("b", "") in
        let concat_prefix a b =
          match a, b with
          | "", "" -> ""
          | "", p -> p
          | p, "" -> p
          | a, b -> sprintf "%s_%s" a b
        in

        let scope (t, p) =
          ( a.scope (t, concat_prefix p "a"),
            Opt (b.scope (t, concat_prefix p "b")) )
        in
        let decode row ctx =
          let a = a.decode row ctx in
          let b = Codec.Primitives.option_decode b.decode row ctx in
          a, b
        in
        {
          decode;
          scope;
          tree = JOIN (a, b, e inner_scope);
          select = [];
          fields =
            forward_fields ~prefix:"a" "a" a.fields
            @ forward_fields ~prefix:"b" "b" b.fields;
        }
    | Select (q, f) ->
        let inner = to_rel q in
        let scope, fields, decode = f (inner.scope ("t", "")) in
        { decode; scope; tree = SUBQUERY inner; fields; select = fields }

  let to_sql q =
    let rel = to_rel q in
    trim_select rel;
    rel.decode, rel.scope, print_rel rel

  let from t = From t
  let where q e = Where (q, e)
  let order_by q e = Order_by (q, e)

  (* let join b e a = Join (a, b, e) *)
  let left_join a b e = Left_join (a, b, e)
  let select q f = Select (q, f)

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

module P = struct
  type _ t =
    | E : 'a expr * string option -> 'a t
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

  let select' q make_scope p =
    Q.select q (fun scope ->
        let p = p scope in
        let fields = fields p in
        make_scope scope, fields, decode p)

  let select q p = select' q (fun _ _ -> ()) p

  let fold q p db ~init ~f =
    let q = select q p in
    let decode, _scope, sql = Q.to_sql q in
    let sql = Containers_pp.Pretty.to_string ~width:79 sql in
    print_endline sql;
    fold_raw sql decode db ~init ~f

  let iter q p db ~f = fold q p db ~init:() ~f:(fun () row -> f row)
end

module Primitives = struct
  include Codec.Primitives

  type string_scope = string expr
  type int_scope = int expr
  type float_scope = float expr
  type bool_scope = bool expr

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
