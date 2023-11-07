open Printf
open Ppxlib
open Ast_builder.Default
open ContainersLabels

module Repr = struct
  type type_decl = {
    name : label loc;
    params : label loc list;
    shape : type_decl_shape;
    loc : location;
  }

  and type_decl_shape =
    | Ts_record of (label loc * type_expr) list
    | Ts_variant of variant_case list
    | Ts_expr of type_expr

  and type_expr' =
    | Te_opaque of Longident.t loc * type_expr list
    | Te_var of label loc
    | Te_tuple of type_expr list
    | Te_polyvariant of polyvariant_case list

  and type_expr = core_type * type_expr'

  and variant_case =
    | Vc_tuple of label loc * type_expr list
    | Vc_record of label loc * (label loc * type_expr) list

  and polyvariant_case =
    | Pvc_construct of label loc * type_expr list
    | Pvc_inherit of Longident.t loc * type_expr list

  exception Not_supported of string

  let not_supported what =
    raise (Not_supported (sprintf "%s are not supported" what))

  let rec of_core_type (typ : Parsetree.core_type) : type_expr =
    match typ.ptyp_desc with
    | Ptyp_tuple ts -> typ, Te_tuple (List.map ts ~f:of_core_type)
    | Ptyp_constr (id, ts) ->
        typ, Te_opaque (id, List.map ts ~f:(fun t -> of_core_type t))
    | Ptyp_variant (fields, Closed, None) ->
        let cs =
          List.map fields ~f:(fun field ->
              match field.prf_desc with
              | Rtag (id, _, ts) ->
                  Pvc_construct (id, List.map ts ~f:of_core_type)
              | Rinherit { ptyp_desc = Ptyp_constr (id, ts); _ } ->
                  Pvc_inherit (id, List.map ts ~f:of_core_type)
              | Rinherit _ -> not_supported "this polyvariant inherit")
        in
        typ, Te_polyvariant cs
    | Ptyp_variant _ -> not_supported "non closed polyvariants"
    | Ptyp_arrow _ -> not_supported "function types"
    | Ptyp_any -> not_supported "type placeholders"
    | Ptyp_var label -> typ, Te_var { txt = label; loc = typ.ptyp_loc }
    | Ptyp_object _ -> not_supported "object types"
    | Ptyp_class _ -> not_supported "class types"
    | Ptyp_poly _ -> not_supported "polymorphic type expressions"
    | Ptyp_package _ -> not_supported "packaged module types"
    | Ptyp_extension _ -> not_supported "extension nodes"
    | Ptyp_alias _ -> not_supported "type aliases"

  let of_type_declaration (td : Parsetree.type_declaration) : type_decl =
    let shape =
      match td.ptype_kind, td.ptype_manifest with
      | Ptype_abstract, None -> not_supported "abstract types"
      | Ptype_abstract, Some t -> Ts_expr (of_core_type t)
      | Ptype_variant ctors, _ ->
          let cs =
            List.map ctors ~f:(fun ctor ->
                match ctor.pcd_args with
                | Pcstr_tuple ts ->
                    Vc_tuple (ctor.pcd_name, List.map ts ~f:of_core_type)
                | Pcstr_record fs ->
                    let fs =
                      List.map fs ~f:(fun f ->
                          f.pld_name, of_core_type f.pld_type)
                    in
                    Vc_record (ctor.pcd_name, fs))
          in
          Ts_variant cs
      | Ptype_record fs, _ ->
          let fs =
            List.map fs ~f:(fun f -> f.pld_name, of_core_type f.pld_type)
          in
          Ts_record fs
      | Ptype_open, _ -> not_supported "open types"
    in
    let params =
      List.map td.ptype_params ~f:(fun (t, _) ->
          match t.ptyp_desc with
          | Ptyp_var name -> { txt = name; loc = t.ptyp_loc }
          | _ -> failwith "type variable is not a variable")
    in
    { name = td.ptype_name; shape; params; loc = td.ptype_loc }

  let te_opaque (n : Longident.t loc) ts =
    ptyp_constr ~loc:n.loc n (List.map ts ~f:fst), Te_opaque (n, ts)

  let decl_to_te_expr decl =
    let loc = decl.loc in
    ptyp_constr ~loc
      { loc; txt = lident decl.name.txt }
      (List.map decl.params ~f:(fun { loc; txt } -> ptyp_var ~loc txt))
end

module Deriving_helper = struct
  let to_lident id = { loc = id.loc; txt = lident id.txt }

  let gen_bindings ~loc prefix n =
    List.split
      (List.init n ~f:(fun i ->
           let id = sprintf "%s_%i" prefix i in
           let patt = ppat_var ~loc { loc; txt = id } in
           let expr = pexp_ident ~loc { loc; txt = lident id } in
           patt, expr))

  let gen_pat_tuple ~loc prefix n =
    let patts, exprs = gen_bindings ~loc prefix n in
    ppat_tuple ~loc patts, exprs

  let gen_pat_list ~loc prefix n =
    let patts, exprs = gen_bindings ~loc prefix n in
    let patt =
      List.fold_left (List.rev patts)
        ~init:[%pat? []]
        ~f:(fun prev patt -> [%pat? [%p patt] :: [%p prev]])
    in
    patt, exprs

  let gen_bindings_record ~loc prefix fs =
    let ps, es =
      List.split
        (List.map fs ~f:(fun (n, _t) ->
             let id = sprintf "%s_%s" prefix n.txt in
             let patt = ppat_var ~loc { loc = n.loc; txt = id } in
             let expr =
               pexp_ident ~loc { loc = n.loc; txt = lident id }
             in
             (to_lident n, patt), expr))
    in
    let ns, ps = List.split ps in
    ns, ps, es

  let gen_pat_record ~loc prefix fs =
    let ns, ps, es = gen_bindings_record ~loc prefix fs in
    ppat_record ~loc (List.combine ns ps) Closed, es

  let ( --> ) pc_lhs pc_rhs = { pc_lhs; pc_rhs; pc_guard = None }

  let name_of_t name = function
    | "t" -> name
    | t -> Printf.sprintf "%s_%s" t name

  let name_loc_of_t name id = { id with txt = name_of_t name id.txt }

  let name_of_longident name_of_t name (lid : Longident.t) =
    match lid with
    | Lident lab -> Longident.Lident (name_of_t name lab)
    | Ldot (lid, lab) -> Longident.Ldot (lid, name_of_t name lab)
    | Lapply (_, _) -> failwith "unable to get name of Lapply"

  let lident_of_t name label =
    let n = name_loc_of_t name label in
    { loc = n.loc; txt = Longident.parse n.txt }

  let ederiver name (lid : Longident.t loc) =
    pexp_ident ~loc:lid.loc
      { loc = lid.loc; txt = name_of_longident name_of_t name lid.txt }
end

type deriver =
  | As_fun of (expression -> expression)
  | As_val of expression

let as_val ~loc deriver x =
  match deriver with As_fun f -> f x | As_val f -> [%expr [%e f] [%e x]]

let as_fun ~loc deriver =
  match deriver with
  | As_fun f -> [%expr fun x -> [%e f [%expr x]]]
  | As_val f -> f

open Repr
open Deriving_helper

class virtual deriving1 =
  object (self)
    method virtual name : string
    method binding_name = self#name
    method deriver_name = ederiver
    method virtual t : loc:location -> core_type -> core_type

    method derive_of_tuple
        : loc:location -> type_expr list -> expression -> expression =
      not_supported "tuple types"

    method derive_of_record
        : loc:location ->
          (label loc * type_expr) list ->
          expression ->
          expression =
      not_supported "record types"

    method derive_of_variant
        : loc:location -> variant_case list -> expression -> expression =
      not_supported "variant types"

    method derive_of_polyvariant
        : loc:location ->
          polyvariant_case list ->
          core_type ->
          expression ->
          expression =
      not_supported "variant types"

    method private derive_type_ref' ~loc name n ts =
      let f = ederiver name n in
      let args =
        List.fold_left (List.rev ts) ~init:[] ~f:(fun args a ->
            let a = as_fun ~loc (self#derive_of_type_expr' ~loc a) in
            (Nolabel, a) :: args)
      in
      As_val (pexp_apply ~loc f args)

    method derive_type_ref ~loc name n ts x =
      as_val ~loc (self#derive_type_ref' ~loc name n ts) x

    method private derive_of_type_expr' ~loc =
      function
      | _, Te_tuple ts -> As_fun (fun x -> self#derive_of_tuple ~loc ts x)
      | _, Te_var id -> As_val (ederiver self#name (to_lident id))
      | _, Te_opaque (n, ts) -> self#derive_type_ref' self#name ~loc n ts
      | t, Te_polyvariant cs ->
          As_fun (fun x -> self#derive_of_polyvariant ~loc cs t x)

    method derive_of_type_expr ~loc repr x =
      as_val ~loc (self#derive_of_type_expr' ~loc repr) x

    method private derive_type_shape ~loc x =
      function
      | Ts_expr t -> as_val ~loc (self#derive_of_type_expr' ~loc t) x
      | Ts_record fs -> self#derive_of_record ~loc fs x
      | Ts_variant cs -> self#derive_of_variant ~loc cs x

    method derive_type_decl ({ name; params; shape; loc } as decl) =
      let expr = self#derive_type_shape ~loc [%expr x] shape in
      let t = Repr.decl_to_te_expr decl in
      let expr = [%expr (fun x -> [%e expr] : [%t self#t ~loc t])] in
      let expr =
        List.fold_left params ~init:expr ~f:(fun body param ->
            pexp_fun ~loc Nolabel None
              (ppat_var ~loc (name_loc_of_t self#name param))
              body)
      in
      [
        value_binding ~loc
          ~pat:(ppat_var ~loc (name_loc_of_t self#binding_name name))
          ~expr;
      ]

    method extension
        : loc:location -> path:label -> core_type -> expression =
      fun ~loc:_ ~path:_ ty ->
        let repr = Repr.of_core_type ty in
        let loc = ty.ptyp_loc in
        as_fun ~loc (self#derive_of_type_expr' ~loc repr)

    method generator
        : ctxt:Expansion_context.Deriver.t ->
          rec_flag * type_declaration list ->
          structure =
      fun ~ctxt (rec_flag, type_decls) ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match List.map type_decls ~f:Repr.of_type_declaration with
        | exception Not_supported msg ->
            [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
        | reprs ->
            let bindings =
              List.flat_map reprs ~f:(fun decl ->
                  self#derive_type_decl decl)
            in
            let rec_flag =
              match bindings with [ _ ] -> Nonrecursive | _ -> rec_flag
            in
            [%str
              [@@@ocaml.warning "-39-11"]

              [%%i pstr_value ~loc rec_flag bindings]]
  end

let derive_decode =
  object (self)
    inherit deriving1
    method name = "decode"

    method t ~loc t =
      [%type: Persistent.ctx * Sqlite3.Data.t array -> [%t t]]

    method! derive_of_tuple ~loc ts x =
      let n = List.length ts in
      let ps, es = gen_bindings ~loc "x" n in
      List.fold_left2 (List.rev ps) (List.rev ts)
        ~init:(pexp_tuple ~loc es) ~f:(fun next p t ->
          [%expr
            let [%p p] = [%e self#derive_of_type_expr ~loc t x] in
            [%e next]])

    method! derive_of_record ~loc fs x =
      let ns, ps, es = gen_bindings_record ~loc "x" fs in
      List.fold_left2 (List.rev ps) (List.rev fs)
        ~init:(pexp_record ~loc (List.combine ns es) None)
        ~f:(fun next p (_, t) ->
          [%expr
            let [%p p] = [%e self#derive_of_type_expr ~loc t x] in
            [%e next]])
  end

let derive_bind =
  object (self)
    inherit deriving1
    method name = "bind"

    method t ~loc t =
      [%type: [%t t] -> Persistent.ctx -> Sqlite3.stmt -> unit]

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

let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]])

let derive_columns =
  object (self)
    inherit deriving1
    method name = "columns"
    method t ~loc _t = [%type: string -> (string * string) list]

    method! derive_of_tuple ~loc ts x =
      let es =
        List.mapi ts ~f:(fun i t ->
            let i = eint ~loc i in
            [%expr
              [%e self#derive_of_type_expr ~loc t [%expr genname [%e i]]]])
      in
      [%expr
        let genname =
          match [%e x] with
          | "" -> fun i -> Printf.sprintf "c%i" i
          | prefix -> fun i -> Printf.sprintf "%s_%i" prefix i
        in
        List.flatten [%e pexp_list ~loc es]]

    method! derive_of_record ~loc fs x =
      let es =
        List.map fs ~f:(fun ((n : label loc), t) ->
            let n = estring ~loc:n.loc n.txt in
            [%expr
              [%e self#derive_of_type_expr ~loc t [%expr genname [%e n]]]])
      in
      [%expr
        let genname =
          match [%e x] with
          | "" -> Fun.id
          | prefix -> fun n -> Printf.sprintf "%s_%s" prefix n
        in
        List.flatten [%e pexp_list ~loc es]]
  end

let codec =
  Deriving.add "codec"
    ~str_type_decl:
      (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt str ->
           derive_decode#generator ~ctxt str
           @ derive_bind#generator ~ctxt str
           @ derive_columns#generator ~ctxt str))

let _ =
  let derive_table ({ name; params; shape = _; loc } : Repr.type_decl) =
    if not (List.is_empty params) then not_supported "type parameters";
    let pat = ppat_var ~loc name in
    let columns = name_loc_of_t "columns" name in
    let bind = lident_of_t "bind" name in
    let decode = lident_of_t "decode" name in
    let columns =
      { loc = columns.loc; txt = Longident.parse columns.txt }
    in
    value_binding ~loc ~pat
      ~expr:
        [%expr
          {
            Persistent.table = [%e estring ~loc name.txt];
            columns = [%e pexp_ident ~loc columns] "";
            decode = [%e pexp_ident ~loc decode];
            bind = [%e pexp_ident ~loc bind];
          }]
  in
  Deriving.add "entity"
    ~str_type_decl:
      (Deriving.Generator.V2.make ~deps:[ codec ] Deriving.Args.empty
         (fun ~ctxt (rec_flag, type_decls) ->
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           match List.map type_decls ~f:Repr.of_type_declaration with
           | exception Not_supported msg ->
               [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
           | reprs -> (
               try
                 let bindings = List.map reprs ~f:derive_table in
                 [%str
                   [@@@ocaml.warning "-39-11"]

                   [%%i pstr_value ~loc rec_flag bindings]]
               with Not_supported msg ->
                 [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ])))
