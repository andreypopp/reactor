open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default

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
        let id = sprintf "%s_%s" prefix n in
        let patt = ppat_var ~loc { loc; txt = id } in
        let expr = pexp_ident ~loc { loc; txt = lident id } in
        ({ loc; txt = lident n }, patt), expr)
  in
  (* TODO: is there unzip/uncombine somewhere? *)
  ppat_record ~loc (List.map xs ~f:fst) Closed, List.map xs ~f:snd

let with_refs ~loc prefix fs inner =
  let gen_name name = sprintf "%s_%s" prefix name in
  let gen_expr name =
    pexp_ident ~loc { loc; txt = lident (gen_name name) }
  in
  List.fold_left (List.rev fs) ~init:(inner gen_expr)
    ~f:(fun next (n, _t) ->
      let patt = ppat_var ~loc { loc; txt = gen_name n } in
      [%expr
        let [%p patt] = ref Stdlib.Option.None in
        [%e next]])

let name_of_longident name_of_t (lid : Longident.t) =
  match lid with
  | Lident lab -> Longident.Lident (name_of_t lab)
  | Ldot (lid, lab) -> Longident.Ldot (lid, name_of_t lab)
  | Lapply (_, _) -> failwith "TODO"

let ( --> ) pc_lhs pc_rhs = { pc_lhs; pc_rhs; pc_guard = None }

module List = struct
  include List

  let rec combine3 xs ys zs =
    match xs, ys, zs with
    | x :: xs, y :: ys, z :: zs -> (x, y, z) :: combine3 xs ys zs
    | [], [], [] -> []
    | _ -> raise (Invalid_argument "list of different lengths")
end
