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

  and type_expr =
    | Te_opaque of Longident.t loc * type_expr list
    | Te_var of label loc
    | Te_tuple of type_expr list
    | Te_polyvariant of polyvariant_case list

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
    | Ptyp_tuple ts -> Te_tuple (List.map ts ~f:of_core_type)
    | Ptyp_constr (id, ts) ->
        Te_opaque (id, List.map ts ~f:(fun t -> of_core_type t))
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
        Te_polyvariant cs
    | Ptyp_variant _ -> not_supported "non closed polyvariants"
    | Ptyp_arrow _ -> not_supported "function types"
    | Ptyp_any -> not_supported "type placeholders"
    | Ptyp_var label -> Te_var { txt = label; loc = typ.ptyp_loc }
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

  let gen_pat_record ~loc prefix fs =
    let xs =
      List.map fs ~f:(fun (n, _t) ->
          let id = sprintf "%s_%s" prefix n.txt in
          let patt = ppat_var ~loc { loc = n.loc; txt = id } in
          let expr = pexp_ident ~loc { loc = n.loc; txt = lident id } in
          (to_lident n, patt), expr)
    in
    (* TODO: is there unzip/uncombine somewhere? *)
    ppat_record ~loc (List.map xs ~f:fst) Closed, List.map xs ~f:snd

  let ( --> ) pc_lhs pc_rhs = { pc_lhs; pc_rhs; pc_guard = None }
end

open Repr

module Deriving1 (S : sig
  val name : string
  val t : loc:location -> core_type -> core_type

  val derive_of_tuple :
    loc:location ->
    (loc:location -> type_expr -> expression -> expression) ->
    type_expr list ->
    expression ->
    expression

  val derive_of_record :
    loc:location ->
    (loc:location -> type_expr -> expression -> expression) ->
    (label loc * type_expr) list ->
    expression ->
    expression

  val derive_of_variant :
    loc:location ->
    (loc:location -> type_expr -> expression -> expression) ->
    variant_case list ->
    expression ->
    expression
end) =
struct
  open Deriving_helper

  let name_of_t = function
    | "t" -> S.name
    | t -> Printf.sprintf "%s_%s" t S.name

  let name_loc_of_t id = { id with txt = name_of_t id.txt }

  let name_of_longident name_of_t (lid : Longident.t) =
    match lid with
    | Lident lab -> Longident.Lident (name_of_t lab)
    | Ldot (lid, lab) -> Longident.Ldot (lid, name_of_t lab)
    | Lapply (_, _) -> failwith "unable to get name of Lapply"

  let ederiver (lid : Longident.t loc) =
    pexp_ident ~loc:lid.loc
      { loc = lid.loc; txt = name_of_longident name_of_t lid.txt }

  type deriver =
    | As_fun of (expression -> expression)
    | As_id of expression

  let app ~loc deriver x =
    match deriver with
    | As_fun f -> f x
    | As_id f -> [%expr [%e f] [%e x]]

  let eta ~loc deriver =
    match deriver with
    | As_fun f -> [%expr fun x -> [%e f [%expr x]]]
    | As_id f -> f

  let rec derive_type_expr ~loc = function
    | Te_tuple ts ->
        As_fun (fun x -> S.derive_of_tuple ~loc eta_derive_type_expr ts x)
    | Te_var id -> As_id (ederiver (to_lident id))
    | Te_opaque (id, args) ->
        let f = ederiver id in
        let args =
          List.fold_left (List.rev args) ~init:[] ~f:(fun args a ->
              let a = eta ~loc (derive_type_expr ~loc a) in
              (Nolabel, a) :: args)
        in
        As_id (pexp_apply ~loc f args)
    | Te_polyvariant _ -> failwith "TODO"

  and eta_derive_type_expr ~loc repr x =
    app ~loc (derive_type_expr ~loc repr) x

  let derive_type_shape ~loc x = function
    | Ts_expr t -> app ~loc (derive_type_expr ~loc t) x
    | Ts_record fs -> S.derive_of_record ~loc eta_derive_type_expr fs x
    | Ts_variant cs -> S.derive_of_variant ~loc eta_derive_type_expr cs x

  let derive_type_decl { name; params; shape; loc } =
    let expr = derive_type_shape ~loc [%expr x] shape in
    let t = ptyp_constr ~loc (to_lident name) [] in
    let expr = [%expr (fun x -> [%e expr] : [%t S.t ~loc t])] in
    let expr =
      List.fold_left params ~init:expr ~f:(fun body param ->
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc (name_loc_of_t param))
            body)
    in
    value_binding ~loc ~pat:(ppat_var ~loc (name_loc_of_t name)) ~expr

  let expand ty =
    let repr = Repr.of_core_type ty in
    let loc = ty.ptyp_loc in
    eta ~loc (derive_type_expr ~loc repr)

  let deriving_args () = Deriving.Args.(empty)

  let deriving () =
    Deriving.Generator.V2.make (deriving_args ())
      (fun ~ctxt (rec_flag, type_decls) ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match List.map type_decls ~f:Repr.of_type_declaration with
        | exception Not_supported msg ->
            [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
        | reprs ->
            let bindings =
              List.map reprs ~f:(fun decl -> derive_type_decl decl)
            in
            let rec_flag =
              match bindings with [ _ ] -> Nonrecursive | _ -> rec_flag
            in
            [ pstr_value ~loc rec_flag bindings ])

  let register () =
    let _ = Deriving.add S.name ~str_type_decl:(deriving ()) in
    let () =
      Driver.register_transformation S.name
        ~rules:
          [
            Context_free.Rule.extension
              (Extension.declare S.name Expression
                 Ast_pattern.(ptyp __)
                 (fun ~loc:_ ~path:_ ty -> expand ty));
          ]
    in
    ()
end
