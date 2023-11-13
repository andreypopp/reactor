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

module Esc = struct
  let add ~escape_singleq ~escape_doubleq b s =
    let len = String.length s in
    let max_idx = len - 1 in
    let flush b start i =
      if start < len then Buffer.add_substring b s start (i - start)
    in
    let rec loop start i =
      if i > max_idx then flush b start i
      else
        let next = i + 1 in
        match String.get s i with
        | '\'' ->
            if escape_singleq then (
              flush b start i;
              Buffer.add_string b "''";
              loop next next)
            else loop start next
        | '"' ->
            if escape_doubleq then (
              flush b start i;
              Buffer.add_string b "\"\"";
              loop next next)
            else loop start next
        | _ -> loop start next
    in
    loop 0 0

  let make escape s =
    let b = Buffer.create (String.length s) in
    escape b s;
    Buffer.contents b

  let id = make (add ~escape_singleq:false ~escape_doubleq:true)
  let render_id v = sprintf {|"%s"|} (id v)
  let print_id v = Containers_pp.text (render_id v)
  let render_col col = render_id col.Codec.column
  let print_col col = print_id col.Codec.column
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
    let b = Buffer.create (String.length v + 2) in
    Buffer.add_char b '\'';
    Esc.add ~escape_singleq:true ~escape_doubleq:false b v;
    Buffer.add_char b '\'';
    let sql = Buffer.contents b in
    { sql; decode = string_codec.decode; cols = [] }

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

  let iif' c t =
    {
      sql = sprintf "iif(%s, %s, NULL)" c.sql t.sql;
      decode = t.decode;
      cols = t.cols;
    }

  let iif c t e =
    {
      sql = sprintf "iif(%s, %s, %s)" c.sql t.sql e.sql;
      decode = t.decode;
      cols = t.cols @ e.cols;
    }

  let col_sql t n =
    let b = Buffer.create (String.length t + String.length n + 5) in
    Buffer.add_char b '"';
    Esc.add ~escape_singleq:false ~escape_doubleq:true b t;
    Buffer.add_char b '"';
    Buffer.add_char b '.';
    Buffer.add_char b '"';
    Esc.add ~escape_singleq:false ~escape_doubleq:true b n;
    Buffer.add_char b '"';
    Buffer.contents b

  let col t col decode =
    { sql = col_sql t col; decode; cols = [ t, col ] }

  let col_opt t col decode =
    { sql = col_sql t col; decode; cols = [ t, col ] }

  let as_col t col e = { e with sql = col_sql t col; cols = [ t, col ] }
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
          textf {|%s %s|} (Esc.render_col col) col.type_)
    in
    let primary_key =
      match primary_key with
      | [] -> []
      | pk ->
          [
            text "PRIMARY KEY"
            ^+ bracket2 "(" (of_list ~sep:comma Esc.print_col pk) ")";
          ]
    in
    let unique =
      match unique with
      | Some unique ->
          (List.map ~f:(fun cols ->
               text "UNIQUE"
               ^+ bracket2 "(" (of_list ~sep:comma Esc.print_col cols) ")"))
            unique
      | None -> []
    in
    group
      (text "CREATE TABLE"
      ^ (if if_not_exists then text " IF NOT EXISTS" else nil)
      ^+ Esc.print_id name
      ^+ bracket2 "("
           (of_list ~sep:comma Fun.id (columns @ primary_key @ unique))
           ")"
      ^ (if strict then text " STRICT" else nil)
      ^ text ";")

  let where_primary_key_sql columns =
    let open Containers_pp in
    of_list
      ~sep:(newline ^ text "AND" ^ sp)
      (fun col -> textf "%s = ?" (Esc.render_col col))
      columns

  let insert_values_sql ~or_replace ~name ~columns =
    let open Containers_pp in
    group
      (text (if or_replace then "INSERT OR REPLACE" else "INSERT")
      ^+ textf "INTO %s" (Esc.render_id name)
      ^+ bracket2 "(" (of_list ~sep:comma Esc.print_col columns) ")"
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
            (fun col -> textf "%s = ?" (Esc.render_id col.Codec.column))
            t.columns)

  let update_sql t =
    let open Containers_pp in
    group
      (textf "UPDATE %s" (Esc.render_id t.table)
      ^+ update_set_sql t
      ^/ group
           (text "WHERE" ^+ where_primary_key_sql t.primary_key_columns))

  let insert_sql t =
    insert_values_sql ~or_replace:false ~name:t.table ~columns:t.columns

  let upsert_sql t =
    insert_values_sql ~or_replace:true ~name:t.table ~columns:t.columns

  let delete_sql' ?where t =
    let open Containers_pp in
    group
      (textf "DELETE FROM %s" (Esc.render_id t.table)
      ^
      match where with
      | None -> nil
      | Some where -> newline ^ group (text "WHERE" ^+ where))

  let delete_sql t =
    delete_sql' t ~where:(where_primary_key_sql t.primary_key_columns)
end

let create t =
  let sql =
    Sql.create_table_sql ~if_not_exists:true ~strict:true
      ~primary_key:t.primary_key_columns
      ~unique:(Option.map List.return t.unique_columns)
      ~name:t.table ~columns:t.columns
  in
  let sql = Containers_pp.Pretty.to_string ~width:79 sql in
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

let delete_where t ~where =
  let where : (bool, _) E.t = where (t.scope ("", "")) in
  let where = Containers_pp.text (E.to_sql where) in
  make_query_with ~sql:(Sql.delete_sql' ~where t) (fun ~ctx:_ ~stmt:_ k ->
      k ())

let update t =
  let bind v ctx stmt =
    t.codec.bind v ctx stmt;
    let pk = t.primary_key v in
    t.primary_key_bind pk ctx stmt
  in
  make_query ~sql:(Sql.update_sql t) ~bind

let fold_raw sql decode db ~init ~f =
  print_endline sql;
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
    | From : ('a, 's, _) table * string -> ('s, 'a) t
    | Where : ('s, 'a) t * string * ('s -> bool expr) -> ('s, 'a) t
    | Order_by : ('s, 'a) t * string * ('s -> order list) -> ('s, 'a) t
    (* | Join : *)
    (*     ('sa, 'a) q * ('sb, 'b) q * ('sa * 'sb -> bool expr) *)
    (*     -> ('sa * 'sb, 'a * 'b) q *)
    | Left_join :
        ('sa, 'a) t
        * string
        * ('sb, 'b) t
        * string
        * ('sa * 'sb -> bool expr)
        -> ('sa * 'sb opt, 'a * 'b option) t
    | Select :
        ('s, 'a) t
        * string
        * ('s -> 's1 make_scope * fields * 'a1 Codec.decode)
        -> ('s1, 'a1) t

  type ('s, 'a) rel = {
    tree : tree;
    scope : 's make_scope;
    fields : (any_expr * string) list;
    mutable select : (any_expr * string) list;
    decode : 'a Codec.decode;
  }

  and tree =
    | SUBQUERY : (_, _) rel * string -> tree
    | FROM : _ table * string -> tree
    | WHERE : (_, _) rel * string * (_, _) E.t -> tree
    | ORDER_BY : (_, _) rel * string * order list -> tree
    | JOIN :
        (_, _) rel * string * (_, _) rel * string * (_, _) E.t
        -> tree

  let rec print_rel : type s a. (s, a) rel -> Containers_pp.t =
   fun rel ->
    let open Containers_pp in
    let select =
      let fields =
        match rel.select with [] -> rel.fields | select -> select
      in
      of_list ~sep:comma
        (fun (Any_expr e, n) ->
          textf "%s AS %s" (E.to_sql e) (Esc.render_id n))
        fields
    in
    group (text "SELECT" ^ nest 2 (newline ^ select))
    ^/ print_tree rel.tree

  and print_tree : tree -> Containers_pp.t =
    let open Containers_pp in
    function
    | SUBQUERY (rel, n) -> print_from rel n
    | FROM (t, n) -> print_from' (Esc.print_id t.table) n
    | JOIN (a, an, b, bn, e) ->
        print_from a an
        ^/ text "LEFT JOIN"
        ^+ bracket2 "(" (print_rel b)
             (sprintf ") AS %s" (Esc.render_id bn))
        ^/ group (text "ON" ^ nest 2 (newline ^ text (E.to_sql e)))
    | WHERE (rel, n, e) ->
        print_from rel n
        ^/ group (text "WHERE" ^ nest 2 (newline ^ text (E.to_sql e)))
    | ORDER_BY (rel, n, os) ->
        print_from rel n
        ^/ group
             (text "ORDER BY"
             ^ nest 2 (newline ^ of_list ~sep:comma print_order os))

  and print_from : type s a. (s, a) rel -> string -> Containers_pp.t =
    let open Containers_pp in
    fun rel name -> print_from' (bracket2 "(" (print_rel rel) ")") name

  and print_from' rel name =
    let open Containers_pp in
    text "FROM" ^+ rel ^+ textf "AS %s" (Esc.render_id name)

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
    | SUBQUERY (inner, n) ->
        mark n inner used_select used_select;
        trim_select inner
    | FROM _ -> ()
    | WHERE (inner, n, e) ->
        let used = used_expr e used_select in
        mark n inner used_select used;
        trim_select inner
    | ORDER_BY (inner, n, fs) ->
        let used =
          List.fold_left fs
            ~f:(fun used -> function
              | Asc e -> used_expr e used | Desc e -> used_expr e used)
            ~init:used_select
        in
        mark n inner used_select used;
        trim_select inner
    | JOIN (a, na, b, nb, e) ->
        let used = used_expr e used_select in
        mark na a used_select used;
        trim_select a;
        mark nb b used_select used;
        trim_select b

  let rename_fields t fs =
    List.map fs ~f:(fun (Any_expr e, n) -> Any_expr (E.as_col t n e), n)

  let forward_fields ?prefix t fs =
    List.map fs ~f:(fun (Any_expr e, n) ->
        let n' =
          match prefix with
          | None -> n
          | Some prefix -> sprintf "%s.%s" prefix n
        in
        Any_expr (E.as_col t n e), n')

  let rec to_rel : type s a. (s, a) t -> (s, a) rel = function
    | From (t, n) ->
        {
          decode = t.codec.decode;
          scope = t.scope;
          tree = FROM (t, n);
          fields = rename_fields n t.fields;
          select = [];
        }
    | Where (q, n, e) ->
        let inner = to_rel q in
        let e = e (inner.scope (n, "")) in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = WHERE (inner, n, e);
          fields = forward_fields n inner.fields;
          select = [];
        }
    | Order_by (q, n, e) ->
        let inner = to_rel q in
        {
          decode = inner.decode;
          scope = inner.scope;
          tree = ORDER_BY (inner, n, e (inner.scope (n, "")));
          fields = forward_fields n inner.fields;
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
    | Left_join (a, na, b, nb, e) ->
        let a = to_rel a in
        let b = to_rel b in
        let inner_scope = a.scope (na, ""), b.scope (nb, "") in
        let concat_prefix a b =
          match a, b with
          | "", "" -> ""
          | "", p -> p
          | p, "" -> p
          | a, b -> sprintf "%s.%s" a b
        in

        let scope (t, p) =
          ( a.scope (t, concat_prefix p na),
            Opt (b.scope (t, concat_prefix p nb)) )
        in
        let decode row ctx =
          let a = a.decode row ctx in
          let b = Codec.Primitives.option_decode b.decode row ctx in
          a, b
        in
        {
          decode;
          scope;
          tree = JOIN (a, na, b, nb, e inner_scope);
          select = [];
          fields =
            forward_fields ~prefix:na na a.fields
            @ forward_fields ~prefix:nb nb b.fields;
        }
    | Select (q, n, f) ->
        let inner = to_rel q in
        let scope, fields, decode = f (inner.scope (n, "")) in
        {
          decode;
          scope;
          tree = SUBQUERY (inner, n);
          fields;
          select = fields;
        }

  let to_sql q =
    let rel = to_rel q in
    trim_select rel;
    rel.decode, rel.scope, print_rel rel

  let from ?n t = From (t, Option.value n ~default:t.table)
  let where ?(n = "t") q e = Where (q, n, e)
  let order_by ?(n = "t") q e = Order_by (q, n, e)

  (* let join b e a = Join (a, b, e) *)
  let left_join ?(na = "a") ?(nb = "b") a b e = Left_join (a, na, b, nb, e)
  let select ?(n = "t") q f = Select (q, n, f)

  let fold db q ~init ~f =
    let decode, _scope, sql = to_sql q in
    let sql = Containers_pp.Pretty.to_string ~width:79 sql in
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

  let select' ?n q make_scope p =
    Q.select ?n q (fun scope ->
        let p = p scope in
        let fields = fields p in
        make_scope scope, fields, decode p)

  let select ?n q p = select' ?n q (fun _ _ -> ()) p

  let fold q p db ~init ~f =
    let q = select q p in
    let decode, _scope, sql = Q.to_sql q in
    let sql = Containers_pp.Pretty.to_string ~width:79 sql in
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
