open Repr
open Ppxlib
open Ast_builder.Default
module Repr = Repr
module Deriving_helper = Deriving_helper

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
    (label * type_expr) list ->
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

  let ederiver ~loc lid =
    pexp_ident ~loc { loc; txt = name_of_longident name_of_t lid }

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
    | Te_var id -> As_id (ederiver ~loc (lident id))
    | Te_opaque (id, args) ->
        let f = ederiver ~loc id in
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
    let t = ptyp_constr ~loc { loc; txt = lident name } [] in
    let expr = [%expr (fun x -> [%e expr] : [%t S.t ~loc t])] in
    let expr =
      List.fold_left params ~init:expr ~f:(fun body param ->
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc { loc; txt = name_of_t param })
            body)
    in
    value_binding ~loc
      ~pat:(ppat_var ~loc { loc; txt = name_of_t name })
      ~expr

  let expand ty =
    let repr = Repr.of_core_type ty in
    let loc = ty.ptyp_loc in
    eta ~loc (derive_type_expr ~loc repr)

  let deriving_args () = Deriving.Args.(empty)

  let deriving () =
    Deriving.Generator.V2.make (deriving_args ())
      (fun ~ctxt (rec_flag, type_decls) ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        let reprs = List.map type_decls ~f:Repr.of_type_declaration in
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
