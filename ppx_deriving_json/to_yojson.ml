open ContainersLabels
open Ppxlib
open Ast_builder.Default

let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]])

let to_json =
  object (self)
    inherit Ppx_deriving_schema.derive_to
    method name = "to_json"
    method t_to ~loc = [%type: Yojson.Basic.t]

    method derive_of_tuple ~loc ts es =
      let es = List.map2 ts es ~f:(self#derive_type_expr ~loc) in
      [%expr `List [%e pexp_list ~loc es]]

    method derive_of_record ~loc fs es =
      let es =
        List.map2 fs es ~f:(fun (n, t) x ->
            [%expr
              [%e estring ~loc:n.loc n.txt],
                [%e self#derive_type_expr ~loc t x]])
      in
      [%expr `Assoc [%e pexp_list ~loc es]]

    method derive_of_variant_case ~loc n ts es =
      [%expr
        `List
          (`String [%e estring ~loc:n.loc n.txt]
          :: [%e
               pexp_list ~loc
                 (List.map2 ts es ~f:(self#derive_type_expr ~loc))])]

    method derive_of_variant_case_record ~loc n fs es =
      [%expr
        `List
          (`String [%e estring ~loc:n.loc n.txt]
          :: [ [%e self#derive_of_record ~loc fs es] ])]
  end
