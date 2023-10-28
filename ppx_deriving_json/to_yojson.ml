open ContainersLabels
open Ppxlib
open Ast_builder.Default

let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]])

include Ppx_deriving_schema.Deriving_to (struct
  let name = "to_json"
  let t_to = let loc = Location.none in [%type: Yojson.Basic.t]

  let derive_of_tuple ~loc derive ts es =
    let es = List.map2 ts es ~f:(derive ~loc) in
    [%expr `List [%e pexp_list ~loc es]]

  let derive_of_record ~loc derive fs es =
    let es =
      List.map2 fs es ~f:(fun (n, t) x ->
          [%expr [%e estring ~loc:n.loc n.txt], [%e derive ~loc t x]])
    in
    [%expr `Assoc [%e pexp_list ~loc es]]

  let derive_of_variant_case ~loc derive n ts es =
    [%expr
      `List
        (`String [%e estring ~loc:n.loc n.txt]
        :: [%e pexp_list ~loc (List.map2 ts es ~f:(derive ~loc))])]

  let derive_of_variant_case_record ~loc derive n fs es =
    [%expr
      `List
        (`String [%e estring ~loc:n.loc n.txt]
        :: [ [%e derive_of_record ~loc derive fs es] ])]
end)
