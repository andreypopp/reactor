open ContainersLabels
open Ppxlib
open Ast_builder.Default

let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]])

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

let to_json =
  Ppx_deriving_schema.deriving_to () ~name:"to_json"
    ~t_to:(fun ~loc -> [%type: Yojson.Basic.t])
    ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
    ~derive_of_variant_case_record
