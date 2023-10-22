open Printf
open ContainersLabels
open Ppxlib

type type_decl = {
  name : label;
  params : label list;
  shape : type_decl_shape;
  loc : location;
}

and type_decl_shape =
  | Ts_record of (label * type_expr) list
  | Ts_variant of variant_case list
  | Ts_expr of type_expr

and type_expr =
  | Te_opaque of Longident.t * type_expr list
  | Te_var of label
  | Te_tuple of type_expr list
  | Te_polyvariant of polyvariant_case

and variant_case =
  | Vc_tuple of label * type_expr list
  | Vc_record of label * (label * type_expr) list

and polyvariant_case =
  | Pvc_construct of label * type_expr list
  | Pvc_inherit of Longident.t

let not_supported what = failwith (sprintf "%s are not supported" what)

let rec of_core_type (typ : Parsetree.core_type) : type_expr =
  match typ.ptyp_desc with
  | Ptyp_tuple ts -> Te_tuple (List.map ts ~f:(fun t -> of_core_type t))
  | Ptyp_constr ({ txt = id; _ }, ts) ->
      Te_opaque (id, List.map ts ~f:(fun t -> of_core_type t))
  | Ptyp_variant (_fields, Closed, None) -> failwith "TODO"
  | Ptyp_variant _ -> not_supported "non closed polyvariants"
  | Ptyp_arrow _ -> not_supported "function types"
  | Ptyp_any -> not_supported "type placeholders"
  | Ptyp_var label -> Te_var label
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
                  Vc_tuple (ctor.pcd_name.txt, List.map ts ~f:of_core_type)
              | Pcstr_record fs ->
                  let fs =
                    List.map fs ~f:(fun f ->
                        f.pld_name.txt, of_core_type f.pld_type)
                  in
                  Vc_record (ctor.pcd_name.txt, fs))
        in
        Ts_variant cs
    | Ptype_record fs, _ ->
        let fs =
          List.map fs ~f:(fun f ->
              f.pld_name.txt, of_core_type f.pld_type)
        in
        Ts_record fs
    | Ptype_open, _ -> not_supported "open types"
  in
  let params =
    List.map td.ptype_params ~f:(fun (t, _) ->
        match t.ptyp_desc with
        | Ptyp_var name -> name
        | _ -> failwith "type variable is not a variable")
  in
  { name = td.ptype_name.txt; shape; params; loc = td.ptype_loc }
