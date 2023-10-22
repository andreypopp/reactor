open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Repr
open Deriving_helper

let name_of_t = function "t" -> "equal" | t -> sprintf "equal_%s" t

let name_of_longident (lid : Longident.t) =
  match lid with
  | Lident lab -> Longident.Lident (name_of_t lab)
  | Ldot (lid, lab) -> Longident.Ldot (lid, name_of_t lab)
  | Lapply (_, _) -> failwith "TODO"

let rec derive_type_expr ~loc x y = function
  | Te_tuple es ->
      let n = List.length es in
      let xpatt, xexprs = gen_pat_tuple ~loc "x" n in
      let ypatt, yexprs = gen_pat_tuple ~loc "y" n in
      let expr = derive_step ~loc (List.combine3 xexprs yexprs es) in
      pexp_match ~loc
        [%expr [%e x], [%e y]]
        [ ppat_tuple ~loc [ xpatt; ypatt ] --> expr ]
  | Te_var _ -> assert false
  | Te_opaque (id, args) ->
      let f = pexp_ident ~loc { loc; txt = name_of_longident id } in
      pexp_apply ~loc f
        (List.map args ~f:(fun t ->
             let f =
               [%expr
                 fun x y ->
                   [%e derive_type_expr ~loc [%expr x] [%expr y] t]]
             in
             Nolabel, f))
  | Te_polyvariant _ -> failwith "TODO"

and derive_step ~loc = function
  | (x, y, t) :: work ->
      let this = derive_type_expr ~loc x y t in
      let rest = derive_step ~loc work in
      [%expr if not [%e this] then false else [%e rest]]
  | _ -> failwith "incompatible type expressions"

let derive_type_shape ~loc x y = function
  | Ts_expr t -> derive_type_expr ~loc x y t
  | Ts_record fs ->
      let names = List.map fs ~f:fst in
      let ts = List.map fs ~f:snd in
      let xpatt, xexprs = gen_pat_record ~loc "x" names in
      let ypatt, yexprs = gen_pat_record ~loc "y" names in
      pexp_match ~loc
        [%expr [%e x], [%e y]]
        [
          ppat_tuple ~loc [ xpatt; ypatt ]
          --> derive_step ~loc (List.combine3 xexprs yexprs ts);
        ]
  | Ts_variant cs ->
      let ctor_pat name pat =
        ppat_construct ~loc { loc; txt = lident name } pat
      in
      let cases =
        List.map cs ~f:(function
          | Vc_record (name, fs) ->
              let ctor_pat pat = ctor_pat name (Some pat) in
              let names = List.map fs ~f:fst in
              let ts = List.map fs ~f:snd in
              let xpatt, xexprs = gen_pat_record ~loc "x" names in
              let ypatt, yexprs = gen_pat_record ~loc "y" names in
              ppat_tuple ~loc [ ctor_pat xpatt; ctor_pat ypatt ]
              --> derive_step ~loc (List.combine3 xexprs yexprs ts)
          | Vc_tuple (name, ts) ->
              let n = List.length ts in
              let ctor_pat pat =
                ctor_pat name (if n = 0 then None else Some pat)
              in
              let xpatt, xexprs = gen_pat_tuple ~loc "x" n in
              let ypatt, yexprs = gen_pat_tuple ~loc "y" n in
              ppat_tuple ~loc [ ctor_pat xpatt; ctor_pat ypatt ]
              --> derive_step ~loc (List.combine3 xexprs yexprs ts))
      in
      pexp_match ~loc [%expr [%e x], [%e y]] cases

let derive_type_decl { name; params; shape; loc } =
  let body = derive_type_shape ~loc [%expr x] [%expr y] shape in
  let body = [%expr fun x y -> [%e body]] in
  let body =
    List.fold_left params ~init:body ~f:(fun body param ->
        pexp_fun ~loc Nolabel None
          (ppat_var ~loc { loc; txt = name_of_t param })
          body)
  in
  [%stri let [%p ppat_var ~loc { loc; txt = name_of_t name }] = [%e body]]

let derive_type_expr ~loc repr =
  [%expr fun x y -> [%e derive_type_expr ~loc [%expr x] [%expr y] repr]]
