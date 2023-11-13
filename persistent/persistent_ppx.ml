open Ppxlib
open Ast_builder.Default
open ContainersLabels
open Ppx_deriving_schema
open Deriving_helper

let pexp_errorf ~loc fmt =
  let open Ast_builder.Default in
  Printf.ksprintf
    (fun msg ->
      pexp_extension ~loc (Location.error_extensionf ~loc "%s" msg))
    fmt

exception Error of location * string

let raise_errorf ~loc fmt =
  Printf.ksprintf (fun msg -> raise (Error (loc, msg))) fmt

let wrap_expand_expression f =
  try f () with
  | Not_supported (loc, msg) -> pexp_errorf ~loc "%s" msg
  | Error (loc, msg) ->
      pexp_extension ~loc (Location.error_extensionf ~loc "%s" msg)

let wrap_expand_structure f =
  try f () with
  | Not_supported (loc, msg) ->
      [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
  | Error (loc, msg) -> [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]

let ext_structure_item ~name expand =
  let pattern =
    let open Ast_pattern in
    let extractor_in_let =
      pstr_value drop (value_binding ~pat:__ ~expr:__ ^:: nil)
    in
    pstr @@ extractor_in_let ^:: nil
  in
  let expand ~ctxt pat expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let rec rewrite e =
      match e.pexp_desc with
      | Pexp_fun (lab, default, pat, e) ->
          pexp_fun ~loc:e.pexp_loc lab default pat (rewrite e)
      | _ -> expand ~ctxt e
    in
    let expr = rewrite expr in
    [%stri let [%p pat] = [%e expr]]
  in
  Context_free.Rule.extension
    (Extension.V3.declare name Extension.Context.structure_item pattern
       expand)

class virtual deriving_type =
  object (self)
    method virtual name : string

    method derive_of_tuple
        : loc:location -> Repr.type_expr list -> core_type =
      not_supported "tuple types"

    method derive_of_record
        : loc:location -> (label loc * Repr.type_expr) list -> core_type =
      not_supported "record types"

    method derive_of_variant
        : loc:location -> Repr.variant_case list -> core_type =
      not_supported "variant types"

    method derive_of_polyvariant
        : loc:location -> Repr.polyvariant_case list -> core_type =
      not_supported "variant types"

    method derive_of_type_expr
        : loc:location -> Repr.type_expr -> core_type =
      fun ~loc t ->
        match t with
        | _, Repr.Te_tuple ts -> self#derive_of_tuple ~loc ts
        | _, Te_var _ -> not_supported ~loc "type variables"
        | _, Te_opaque (n, ts) ->
            if not (List.is_empty ts) then
              not_supported ~loc "type params"
            else
              let n = map_loc (derive_of_longident self#name) n in
              ptyp_constr ~loc n []
        | _, Te_polyvariant cs -> self#derive_of_polyvariant ~loc cs

    method private derive_type_shape ~(loc : location) =
      function
      | Repr.Ts_expr t -> self#derive_of_type_expr ~loc t
      | Ts_record fs -> self#derive_of_record ~loc fs
      | Ts_variant cs -> self#derive_of_variant ~loc cs

    method derive_type_decl { Repr.name; params; shape; loc }
        : type_declaration list =
      let manifest = self#derive_type_shape ~loc shape in
      if not (List.is_empty params) then not_supported ~loc "type params"
      else
        [
          type_declaration ~loc
            ~name:(map_loc (derive_of_label self#name) name)
            ~manifest:(Some manifest) ~cstrs:[] ~private_:Public
            ~kind:Ptype_abstract ~params:[];
        ]

    method generator
        : ctxt:Expansion_context.Deriver.t ->
          rec_flag * type_declaration list ->
          structure =
      fun ~ctxt (_rec_flag, type_decls) ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match List.map type_decls ~f:Repr.of_type_declaration with
        | exception Not_supported (loc, msg) ->
            [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
        | reprs ->
            let type_decls =
              List.flat_map reprs ~f:(fun decl ->
                  self#derive_type_decl decl)
            in
            [%str [%%i pstr_type ~loc Recursive type_decls]]
  end

let with_genname_field ~loc col body =
  [%expr
    let genname =
      match [%e col] with
      | "" -> fun n -> n
      | prefix -> fun n -> Printf.sprintf "%s.%s" prefix n
    in
    [%e body [%expr genname]]]

let with_genname_idx ~loc col body =
  [%expr
    let genname =
      match [%e col] with
      | "" -> fun i -> Printf.sprintf "_%i" i
      | prefix -> fun i -> Printf.sprintf "%s._%i" prefix i
    in
    [%e body [%expr genname]]]

let derive_scope_type =
  object (self)
    inherit deriving_type
    method name = "scope"

    method! derive_of_record
        : loc:location -> (label loc * Repr.type_expr) list -> core_type =
      fun ~loc fs ->
        let fs =
          List.map fs ~f:(fun (n, t) ->
              let loc = n.loc in
              let t = self#derive_of_type_expr ~loc t in
              {
                pof_desc = Otag (n, t);
                pof_loc = loc;
                pof_attributes = [];
              })
        in
        ptyp_object ~loc fs Closed

    method! derive_of_tuple
        : loc:location -> Repr.type_expr list -> core_type =
      fun ~loc ts ->
        let ts = List.map ts ~f:(self#derive_of_type_expr ~loc) in
        ptyp_tuple ~loc ts
  end

class virtual defined_via =
  object (self)
    method virtual via_name : string

    method derive_type_ref_name name lid =
      pexp_field ~loc:lid.loc
        (pexp_ident ~loc:lid.loc
           (map_loc (derive_of_longident self#via_name) lid))
        { txt = lident name; loc = lid.loc }
  end

let derive_scope =
  let match_table ~loc x f =
    match gen_pat_tuple ~loc "x" 2 with
    | p, [ t; c ] -> pexp_match ~loc x [ p --> f (t, c) ]
    | _, _ -> assert false
  in
  object (self)
    inherit deriving1
    inherit! defined_via
    method name = "scope"
    method via_name = "meta"

    method t ~loc name _t =
      let id = map_loc (derive_of_label derive_scope_type#name) name in
      let id = map_loc lident id in
      let scope = ptyp_constr ~loc id [] in
      [%type: string * string -> [%t scope]]

    method! derive_of_tuple ~loc ts x =
      match_table ~loc x @@ fun (tbl, col) ->
      with_genname_idx ~loc col @@ fun genname ->
      let es =
        List.mapi ts ~f:(fun idx t ->
            let idx = eint ~loc idx in
            self#derive_of_type_expr ~loc t
              [%expr [%e tbl], [%e genname] [%e idx]])
      in
      pexp_tuple ~loc es

    method! derive_of_record ~loc fs x =
      match_table ~loc x @@ fun (tbl, col) ->
      with_genname_field ~loc col @@ fun genname ->
      let fields =
        List.map fs ~f:(fun (n, t) ->
            let loc = n.loc in
            let col' = estring ~loc n.txt in
            let e =
              self#derive_of_type_expr ~loc t
                [%expr [%e tbl], [%e genname] [%e col']]
            in
            {
              pcf_desc = Pcf_method (n, Public, Cfk_concrete (Fresh, e));
              pcf_loc = loc;
              pcf_attributes = [];
            })
      in
      pexp_object ~loc (class_structure ~self:(ppat_any ~loc) ~fields)
  end

let derive_decode =
  object (self)
    inherit deriving1
    inherit! defined_via
    method via_name = "codec"
    method name = "decode"

    method t ~loc _name t =
      [%type: Sqlite3.Data.t array -> Persistent.Codec.ctx -> [%t t]]

    method! derive_of_tuple ~loc ts x =
      let n = List.length ts in
      let ps, e = gen_tuple ~loc "x" n in
      let e =
        List.fold_left2 (List.rev ps) (List.rev ts) ~init:e
          ~f:(fun next p t ->
            [%expr
              let [%p p] = [%e self#derive_of_type_expr ~loc t x] ctx in
              [%e next]])
      in
      [%expr fun ctx -> [%e e]]

    method! derive_of_record ~loc fs x =
      let ps, e = gen_record ~loc "x" fs in
      let e =
        List.fold_left2 (List.rev ps) (List.rev fs) ~init:e
          ~f:(fun next p (_, t) ->
            [%expr
              let [%p p] = [%e self#derive_of_type_expr ~loc t x] ctx in
              [%e next]])
      in
      [%expr fun ctx -> [%e e]]
  end

let derive_bind =
  object (self)
    inherit deriving1
    inherit! defined_via
    method via_name = "codec"
    method name = "bind"
    method t ~loc _name t = [%type: [%t t] Persistent.Codec.bind]

    method! derive_of_tuple ~loc ts x =
      let n = List.length ts in
      let p, es = gen_pat_tuple ~loc "x" n in
      let e =
        List.fold_left2 (List.rev es) (List.rev ts) ~init:[%expr ()]
          ~f:(fun next e t ->
            [%expr
              [%e self#derive_of_type_expr ~loc t e] ctx stmt;
              [%e next]])
      in
      [%expr fun ctx stmt -> [%e pexp_match ~loc x [ p --> e ]]]

    method! derive_of_record ~loc fs x =
      let p, es = gen_pat_record ~loc "x" fs in
      let e =
        List.fold_left2 (List.rev es) (List.rev fs) ~init:[%expr ()]
          ~f:(fun next e (_, t) ->
            [%expr
              [%e self#derive_of_type_expr ~loc t e] ctx stmt;
              [%e next]])
      in
      [%expr fun ctx stmt -> [%e pexp_match ~loc x [ p --> e ]]]
  end

let derive_columns =
  object (self)
    inherit deriving1
    inherit! defined_via
    method via_name = "codec"
    method name = "columns"

    method t ~loc _name _t =
      [%type: string -> Persistent.Codec.column list]

    method! derive_of_tuple ~loc ts x =
      with_genname_idx ~loc x @@ fun genname ->
      let es =
        List.mapi ts ~f:(fun i t ->
            let i = eint ~loc i in
            [%expr
              [%e
                self#derive_of_type_expr ~loc t
                  [%expr [%e genname] [%e i]]]])
      in
      [%expr Stdlib.List.flatten [%e pexp_list ~loc es]]

    method! derive_of_record ~loc fs x =
      with_genname_field ~loc x @@ fun genname ->
      let es =
        List.map fs ~f:(fun ((n : label loc), t) ->
            let loc = n.loc in
            let n = estring ~loc n.txt in
            let es =
              self#derive_of_type_expr ~loc t [%expr [%e genname] [%e n]]
            in
            [%expr
              Stdlib.List.map
                (fun col ->
                  { col with Persistent.Codec.field = Some [%e n] })
                [%e es]])
      in
      [%expr Stdlib.List.flatten [%e pexp_list ~loc es]]
  end

let derive_fields =
  object (self)
    inherit deriving1
    inherit! defined_via
    method via_name = "meta"
    method name = "fields"

    method t ~loc _name _t =
      [%type: string -> (Persistent.any_expr * string) list]

    method! derive_of_tuple ~loc ts x =
      with_genname_idx ~loc x @@ fun genname ->
      let es =
        List.mapi ts ~f:(fun i t ->
            let i = eint ~loc i in
            [%expr
              [%e
                self#derive_of_type_expr ~loc t
                  [%expr [%e genname] [%e i]]]])
      in
      [%expr Stdlib.List.flatten [%e pexp_list ~loc es]]

    method! derive_of_record ~loc fs x =
      with_genname_field ~loc x @@ fun genname ->
      let es =
        List.map fs ~f:(fun ((n : label loc), t) ->
            let n = estring ~loc:n.loc n.txt in
            [%expr
              [%e
                self#derive_of_type_expr ~loc t
                  [%expr [%e genname] [%e n]]]])
      in
      [%expr Stdlib.List.flatten [%e pexp_list ~loc es]]
  end

let codec =
  let derive_codec
      ({ name; params; shape = _; loc } as decl : Repr.type_decl) =
    if not (List.is_empty params) then
      not_supported ~loc "type parameters";
    let derive deriver =
      let id = map_loc (derive_of_label deriver#name) name in
      pexp_ident ~loc (map_loc lident id)
    in
    let open_struct stris =
      pstr_open ~loc
        (open_infos ~loc ~override:Fresh
           ~expr:(pmod_structure ~loc stris))
    in
    [
      pstr_type ~loc Recursive (derive_scope_type#derive_type_decl decl);
      open_struct
        [
          pstr_value ~loc Nonrecursive
            (derive_decode#derive_type_decl decl);
          pstr_value ~loc Nonrecursive (derive_bind#derive_type_decl decl);
          pstr_value ~loc Nonrecursive
            (derive_columns#derive_type_decl decl);
          pstr_value ~loc Nonrecursive
            (derive_fields#derive_type_decl decl);
          pstr_value ~loc Nonrecursive
            (derive_scope#derive_type_decl decl);
        ];
      pstr_value ~loc Nonrecursive
        [
          value_binding ~loc
            ~pat:(ppat_var ~loc (map_loc (derive_of_label "codec") name))
            ~expr:
              [%expr
                {
                  Persistent.Codec.columns = [%e derive derive_columns];
                  decode = [%e derive derive_decode];
                  bind = [%e derive derive_bind];
                }];
          value_binding ~loc
            ~pat:(ppat_var ~loc (map_loc (derive_of_label "meta") name))
            ~expr:
              [%expr
                {
                  Persistent.scope = [%e derive derive_scope];
                  fields = [%e derive derive_fields];
                }];
        ];
    ]
  in
  Deriving.add "codec"
    ~str_type_decl:
      (Deriving.Generator.V2.make Deriving.Args.empty
         (fun ~ctxt (_rec_flag, type_decls) ->
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           match List.map type_decls ~f:Repr.of_type_declaration with
           | exception Not_supported (loc, msg) ->
               [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
           | reprs -> (
               try
                 let str = List.flat_map reprs ~f:derive_codec in
                 [%stri [@@@ocaml.warning "-39-11"]] :: str
               with Not_supported (loc, msg) ->
                 [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ])))

let primary_key =
  Attribute.declare_with_attr_loc "persistent.primary_key"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    (fun ~attr_loc -> attr_loc)

let table_args =
  let open Deriving.Args in
  let cols = pexp_tuple (many (pexp_ident __')) in
  let col = map1 (pexp_ident __') ~f:List.return in
  empty +> arg "unique" (cols ||| col) +> arg "name" (estring __)

let _ =
  let derive_table ~name:table_name ~unique
      (td, { Repr.name; params; shape = _; loc }) =
    let table_name =
      match table_name, name.txt with
      | None, "t" ->
          raise_errorf ~loc
            "missing table name, specify with ~name argument"
      | None, txt -> { loc = name.loc; txt }
      | Some txt, _ -> { loc = name.loc; txt }
    in
    let fields =
      match td.ptype_kind with
      | Ptype_record fs -> fs
      | _ -> raise_errorf ~loc "not a record"
    in
    if not (List.is_empty params) then
      not_supported ~loc "type parameters";
    let derive ?(name = name) what =
      let id = map_loc (derive_of_label what) name in
      pexp_ident ~loc (map_loc lident id)
    in
    let derive_pat ?(name = name) what =
      let id = map_loc (derive_of_label what) name in
      ppat_var ~loc id
    in
    let derive_type what =
      let id = map_loc (derive_of_label what) name in
      ptyp_constr ~loc (map_loc lident id) []
    in
    let table = pexp_ident ~loc (map_loc lident name) in
    let ( primary_key_name,
          primary_key_project,
          primary_key_field,
          primary_key_type ) =
      let pk =
        List.filter_map fields ~f:(fun f ->
            match Attribute.get primary_key f with
            | None -> None
            | Some loc ->
                Some
                  ( loc,
                    f.pld_name,
                    estring ~loc f.pld_name.txt,
                    f.pld_type ))
      in
      match pk with
      | [] -> raise_errorf ~loc "missing [@primary_key] annotation"
      | [ (loc, label, pk, pk_type) ] ->
          ( label.txt,
            [%expr
              fun row ->
                [%e pexp_field ~loc [%expr row] (map_loc lident label)]],
            [%expr Some [%e pk]],
            pk_type )
      | _first :: (loc, _, _, _) :: _ ->
          raise_errorf ~loc
            "multiple [@primary_key] annotations are not allowed"
    in
    let optionals =
      (* TODO: read optionals from fields as well *)
      match primary_key_type with
      | [%type: int] -> [ primary_key_name ]
      | _ -> []
    in
    let insert =
      let rev_fields = List.rev fields in
      let bind =
        List.fold_left rev_fields ~init:[%expr ()] ~f:(fun next f ->
            let e = pexp_ident ~loc (map_loc lident f.pld_name) in
            let bind x =
              derive_bind#derive_of_type_expr ~loc
                (Repr.of_core_type f.pld_type)
                x
            in
            let bind =
              if List.mem f.pld_name.txt optionals then
                [%expr
                  Persistent.Primitives.option_bind
                    (fun x -> [%e bind [%expr x]])
                    [%e e]]
              else bind e
            in
            [%expr
              [%e bind] ctx stmt;
              [%e next]])
      in
      let body =
        List.fold_left rev_fields
          ~init:
            [%expr
              fun () ->
                [%e bind];
                k ()]
          ~f:(fun body f ->
            let label =
              if List.mem f.pld_name.txt optionals then
                Optional f.pld_name.txt
              else Labelled f.pld_name.txt
            in
            pexp_fun ~loc label None (ppat_var ~loc f.pld_name) body)
      in
      [%expr fun ~ctx ~stmt k -> [%e body]]
    in
    [
      pstr_value ~loc Nonrecursive
        [
          value_binding ~loc ~pat:(ppat_var ~loc name)
            ~expr:
              [%expr
                let codec = [%e derive "codec"] in
                let meta = [%e derive "meta"] in
                let columns = codec.Persistent.Codec.columns "" in
                let unique_columns =
                  [%e
                    match unique with
                    | None -> [%expr None]
                    | Some unique ->
                        [%expr
                          let unique = [%e pexp_list ~loc unique] in
                          Some
                            (Stdlib.List.filter
                               (fun col ->
                                 match col.Persistent.Codec.field with
                                 | None -> false
                                 | Some field ->
                                     Stdlib.List.mem field unique)
                               columns)]]
                in
                let primary_key = [%e primary_key_project] in
                let primary_key_columns =
                  Stdlib.List.filter
                    (fun col ->
                      col.Persistent.Codec.field = [%e primary_key_field])
                    columns
                in
                let primary_key_bind x =
                  [%e
                    derive_bind#derive_of_type_expr ~loc
                      (Repr.of_core_type primary_key_type)
                      [%expr x]]
                in
                ({
                   Persistent.table =
                     [%e estring ~loc:table_name.loc table_name.txt];
                   codec;
                   unique_columns;
                   primary_key_columns;
                   primary_key_bind;
                   primary_key;
                   fields = meta.fields "";
                   scope = meta.scope;
                   columns;
                 }
                  : ( [%t ptyp_constr ~loc (map_loc lident name) []],
                      [%t derive_type "scope"],
                      [%t primary_key_type] )
                    Persistent.table)];
        ];
      [%stri
        let [%p derive_pat "insert"] =
          Persistent.make_query_with
            ~sql:(Persistent.Sql.insert_sql [%e table])
            [%e insert]];
      [%stri
        let [%p derive_pat "upsert"] =
          Persistent.make_query_with
            ~sql:(Persistent.Sql.upsert_sql [%e table])
            [%e insert]];
      [%stri let [%p derive_pat "delete"] = Persistent.delete [%e table]];
      [%stri let [%p derive_pat "update"] = Persistent.update [%e table]];
    ]
  in
  Deriving.add "table"
    ~str_type_decl:
      (Deriving.Generator.V2.make ~deps:[ codec ] table_args
         (fun ~ctxt (_rec_flag, type_decls) unique name ->
           wrap_expand_structure @@ fun () ->
           let unique =
             Option.map
               (List.map ~f:(function
                 | { txt = Lident txt; loc } -> estring ~loc txt
                 | { txt = _; loc } -> raise_errorf ~loc "not a column"))
               unique
           in
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           let reprs =
             List.map type_decls ~f:(fun td ->
                 td, Repr.of_type_declaration td)
           in
           let str =
             List.flat_map reprs ~f:(derive_table ~name ~unique)
           in
           [%stri [@@@ocaml.warning "-39-11"]] :: str))

module Expr_form = struct
  let expand ~ctxt:_ (e : expression) =
    let rec rewrite e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_ident { txt = Lident "="; _ } -> [%expr Persistent.E.( = )]
      | Pexp_ident { txt = Lident "&&"; _ } -> [%expr Persistent.E.( && )]
      | Pexp_ident { txt = Lident "||"; _ } -> [%expr Persistent.E.( || )]
      | Pexp_ident { txt = Lident ">"; _ } -> [%expr Persistent.E.( > )]
      | Pexp_ident { txt = Lident "<"; _ } -> [%expr Persistent.E.( < )]
      | Pexp_ident { txt = Lident "<="; _ } -> [%expr Persistent.E.( <= )]
      | Pexp_ident { txt = Lident ">="; _ } -> [%expr Persistent.E.( >= )]
      | Pexp_ident { txt = Lident "+"; _ } -> [%expr Persistent.E.( + )]
      | Pexp_ident { txt = Lident "-"; _ } -> [%expr Persistent.E.( - )]
      | Pexp_ident { txt = Lident "*"; _ } -> [%expr Persistent.E.( * )]
      | Pexp_ident { txt = Lident "/"; _ } -> [%expr Persistent.E.( / )]
      | Pexp_ident _ -> e
      | Pexp_field (e, { txt = Lident n; loc = nloc }) ->
          pexp_send ~loc:nloc (rewrite e) { txt = n; loc = nloc }
      | Pexp_apply (e, args) ->
          pexp_apply ~loc (rewrite e)
            (List.map args ~f:(fun (l, e) -> l, rewrite e))
      | Pexp_constant (Pconst_integer _) ->
          [%expr Persistent.E.int [%e e]]
      | Pexp_constant (Pconst_char _) -> [%expr Persistent.E.char [%e e]]
      | Pexp_constant (Pconst_string (_, _, _)) ->
          [%expr Persistent.E.string [%e e]]
      | Pexp_constant (Pconst_float (_, _)) ->
          [%expr Persistent.E.float [%e e]]
      | Pexp_construct ({ txt = Lident "true"; loc }, None) ->
          [%expr Persistent.E.bool true]
      | Pexp_construct ({ txt = Lident "false"; loc }, None) ->
          [%expr Persistent.E.bool false]
      | Pexp_ifthenelse (c, t, None) ->
          [%expr Persistent.E.iif' [%e rewrite c] [%e rewrite t]]
      | Pexp_ifthenelse (c, t, Some e) ->
          [%expr
            Persistent.E.iif [%e rewrite c] [%e rewrite t] [%e rewrite e]]
      | Pexp_field _
      | Pexp_let (_, _, _)
      | Pexp_function _
      | Pexp_fun (_, _, _, _)
      | Pexp_match (_, _)
      | Pexp_try (_, _)
      | Pexp_tuple _ | Pexp_construct _
      | Pexp_variant (_, _)
      | Pexp_record (_, _)
      | Pexp_setfield (_, _, _)
      | Pexp_array _
      | Pexp_sequence (_, _)
      | Pexp_while (_, _)
      | Pexp_for (_, _, _, _, _)
      | Pexp_constraint (_, _)
      | Pexp_coerce (_, _, _)
      | Pexp_send (_, _)
      | Pexp_new _
      | Pexp_setinstvar (_, _)
      | Pexp_override _
      | Pexp_letmodule (_, _, _)
      | Pexp_letexception (_, _)
      | Pexp_assert _ | Pexp_lazy _
      | Pexp_poly (_, _)
      | Pexp_object _
      | Pexp_newtype (_, _)
      | Pexp_pack _
      | Pexp_open (_, _)
      | Pexp_letop _ | Pexp_extension _ | Pexp_unreachable ->
          pexp_errorf ~loc "this expression is not supported"
    in
    wrap_expand_expression @@ fun () -> rewrite e

  let ext =
    let pattern =
      let open Ast_pattern in
      single_expr_payload __
    in
    Context_free.Rule.extension
      (Extension.V3.declare "expr" Extension.Context.expression pattern
         expand)

  let ext' = ext_structure_item ~name:"expr" expand
end

module Query_form = struct
  let rec unroll acc e =
    match e.pexp_desc with
    | Pexp_sequence (a, b) -> unroll (a :: acc) b
    | _ -> e :: acc

  let pexp_slot' ~loc names e =
    [%expr fun [@ocaml.warning "-27"] [%p names] -> [%e e]]

  let pexp_opt ~loc e =
    match e with None -> [%expr None] | Some e -> [%expr Some [%e e]]

  let name_of id =
    match id.pexp_desc with
    | Pexp_ident { txt = Lident txt; loc } ->
        ppat_var ~loc { txt; loc }, estring ~loc txt
    | Pexp_ident { txt = Ldot (_, txt); loc } ->
        ppat_var ~loc { txt; loc }, estring ~loc txt
    | _ -> raise_errorf ~loc:id.pexp_loc "only identifiers are allowed"

  let rec expand' ~ctxt e =
    let rec rewrite ~alias names prev q =
      let loc = q.pexp_loc in
      match q with
      | [%expr from [%e? id]] ->
          let names, alias' = name_of id in
          let alias = Option.value alias ~default:alias' in
          ( names,
            Some alias,
            [%expr Persistent.Q.from ~n:[%e alias] [%e id]] )
      | [%expr where [%e? e]] ->
          ( names,
            alias,
            [%expr
              Persistent.Q.where ?n:[%e pexp_opt ~loc alias] [%e prev]
                [%e pexp_slot' ~loc names (Expr_form.expand ~ctxt e)]] )
      | [%expr order_by [%e? fs]] ->
          let fs =
            let fs =
              match fs.pexp_desc with Pexp_tuple fs -> fs | _ -> [ fs ]
            in
            List.map fs ~f:(function
              | [%expr desc [%e? e]] ->
                  [%expr Persistent.Q.desc [%e Expr_form.expand ~ctxt e]]
              | [%expr asc [%e? e]] ->
                  [%expr Persistent.Q.asc [%e Expr_form.expand ~ctxt e]]
              | e ->
                  raise_errorf ~loc:e.pexp_loc
                    "should have form 'desc e' or 'asc e'")
          in
          let e = pexp_list ~loc fs in
          ( names,
            alias,
            [%expr
              Persistent.Q.order_by ?n:[%e pexp_opt ~loc alias] [%e prev]
                [%e pexp_slot' ~loc names e]] )
      | [%expr left_join [%e? q] [%e? e]] ->
          let aalias = alias in
          let qnames, balias, q = expand' ~ctxt q in
          let names = [%pat? [%p names], [%p qnames]] in
          let alias = Some [%expr "q"] in
          ( names,
            alias,
            [%expr
              Persistent.Q.left_join ?na:[%e pexp_opt ~loc aalias]
                ?nb:[%e pexp_opt ~loc balias] [%e prev] [%e q]
                [%e pexp_slot' ~loc names (Expr_form.expand ~ctxt e)]] )
      | { pexp_desc = Pexp_tuple xs; _ } ->
          let xs =
            List.map xs ~f:(function
              | [%expr nullable [%e? exp]] ->
                  `null, Expr_form.expand ~ctxt exp
              | exp -> `not_null, Expr_form.expand ~ctxt exp)
          in
          let make_scope =
            let xs =
              List.mapi xs ~f:(fun i (_, e) ->
                  let n = estring ~loc (Printf.sprintf "_%i" i) in
                  [%expr Persistent.E.as_col t [%e n] [%e e]])
            in
            pexp_slot' ~loc names
              [%expr fun (t, _p) -> [%e pexp_tuple ~loc xs]]
          in
          let ps, e = gen_tuple ~loc "col" (List.length xs) in
          let x, xs =
            match List.combine ps xs with
            | [] -> assert false
            | x :: xs -> x, xs
          in
          let make txt (pat, exp) =
            let exp =
              match exp with
              | `null, exp -> [%expr Persistent.P.get_opt [%e exp]]
              | `not_null, exp -> [%expr Persistent.P.get [%e exp]]
            in
            binding_op ~loc ~op:{ loc; txt } ~pat ~exp
          in
          let e =
            pexp_letop ~loc
              (letop ~body:e ~let_:(make "let+" x)
                 ~ands:(List.map xs ~f:(make "and+")))
          in
          let e =
            [%expr
              let open Persistent.P in
              [%e e]]
          in
          let e =
            [%expr
              Persistent.P.select' ?n:[%e pexp_opt ~loc alias] [%e prev]
                [%e make_scope] [%e pexp_slot' ~loc names e]]
          in
          [%pat? here], alias, e
      | { pexp_desc = Pexp_record (fs, None); _ } ->
          let fs =
            List.map fs ~f:(fun (n, x) ->
                match n.txt with
                | Lident txt -> { txt; loc = n.loc }, x
                | _ -> raise_errorf ~loc "invalid select")
          in
          let ps, e = gen_tuple ~loc "c" (List.length fs) in
          let xs =
            List.map fs ~f:(fun (n, x) ->
                match x with
                | [%expr nullable [%e? exp]] ->
                    n, (`null, Expr_form.expand ~ctxt exp)
                | exp -> n, (`not_null, Expr_form.expand ~ctxt exp))
          in
          let make_scope =
            let fields =
              List.map xs ~f:(fun (n, (_, e)) ->
                  let ns = estring ~loc:n.loc n.txt in
                  let e = [%expr Persistent.E.as_col t [%e ns] [%e e]] in
                  {
                    pcf_desc =
                      Pcf_method (n, Public, Cfk_concrete (Fresh, e));
                    pcf_loc = loc;
                    pcf_attributes = [];
                  })
            in
            pexp_slot' ~loc names
              [%expr
                fun (t, _p) ->
                  [%e
                    pexp_object ~loc
                      (class_structure ~self:(ppat_any ~loc) ~fields)]]
          in
          let x, xs =
            match List.combine ps xs with
            | [] -> assert false
            | x :: xs -> x, xs
          in
          let e =
            let make txt (pat, (name, exp)) =
              let name = estring ~loc:name.loc name.txt in
              let exp =
                match exp with
                | `null, exp ->
                    [%expr Persistent.P.get_opt ~name:[%e name] [%e exp]]
                | `not_null, exp ->
                    [%expr Persistent.P.get ~name:[%e name] [%e exp]]
              in
              binding_op ~loc ~op:{ loc; txt } ~pat ~exp
            in
            pexp_letop ~loc
              (letop ~body:e ~let_:(make "let+" x)
                 ~ands:(List.map xs ~f:(make "and+")))
          in
          let e =
            [%expr
              let open Persistent.P in
              [%e e]]
          in
          let e =
            [%expr
              Persistent.P.select' ?n:[%e pexp_opt ~loc alias] [%e prev]
                [%e make_scope] [%e pexp_slot' ~loc names e]]
          in
          [%pat? here], alias, e
      | { pexp_desc = Pexp_ident _; _ } as q ->
          let name, alias' = name_of q in
          name, Some (Option.value alias ~default:alias'), q
      | [%expr [%e? name] = [%e? rhs]] ->
          let _name, _alias, rhs = rewrite ~alias names prev rhs in
          let name, alias = name_of name in
          name, Some alias, rhs
      | [%expr
          let [%p? pat] = [%e? ocamlish] in
          [%e? next]] ->
          let name, alias, next = rewrite ~alias names prev next in
          ( name,
            alias,
            [%expr
              let [%p pat] = [%e ocamlish] in
              [%e next]] )
      | _ -> raise_errorf ~loc "unknown query form"
    in
    match List.rev (unroll [] e) with
    | [] ->
        raise_errorf
          ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
          "empty query"
    | q :: qs ->
        let loc = Expansion_context.Extension.extension_point_loc ctxt in
        List.fold_left qs
          ~init:(rewrite ~alias:None [%pat? ()] [%expr ()] q)
          ~f:(fun (names, alias, prev) e ->
            let names, alias, e = rewrite ~alias names prev e in
            names, alias, e)

  let expand ~ctxt e =
    wrap_expand_expression @@ fun () ->
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match e with
    | [%expr
        let [%p? p] = [%e? e] in
        [%e? body]] ->
        let _, _, e = expand' ~ctxt e in
        [%expr
          let [%p p] = [%e e] in
          [%e body]]
    | e ->
        let _, _, e = expand' ~ctxt e in
        e

  let ext =
    let pattern =
      let open Ast_pattern in
      single_expr_payload __
    in
    Context_free.Rule.extension
      (Extension.V3.declare "query" Extension.Context.expression pattern
         expand)

  let ext' = ext_structure_item ~name:"query" expand
end

let () =
  Driver.register_transformation
    ~rules:
      [ Expr_form.ext; Expr_form.ext'; Query_form.ext; Query_form.ext' ]
    "persistent_ppx"
