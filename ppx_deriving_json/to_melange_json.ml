open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Deriving_helper

let as_json ~loc x = [%expr (Obj.magic [%e x] : Js.Json.t)]

let to_json =
  object (self)
    inherit Ppx_deriving_schema.derive_to
    method name = "to_json"
    method t_to ~loc = [%type: Js.Json.t]

    method derive_of_tuple ~loc ts es =
      as_json ~loc
        (pexp_array ~loc
           (List.map2 ts es ~f:(self#derive_of_type_expr ~loc)))

    method derive_of_record ~loc fs es =
      let fs =
        List.map2 fs es ~f:(fun (n, t) x ->
            let this = self#derive_of_type_expr ~loc t x in
            to_lident n, this)
      in
      let record = pexp_record ~loc fs None in
      as_json ~loc [%expr [%mel.obj [%e record]]]

    method derive_of_variant_case ~loc n ts es =
      let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
      let es = List.map2 ts es ~f:(self#derive_of_type_expr ~loc) in
      as_json ~loc (pexp_array ~loc (tag :: es))

    method derive_of_variant_case_record ~loc n fs es =
      let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
      let es = [ self#derive_of_record ~loc fs es ] in
      as_json ~loc (pexp_array ~loc (tag :: es))
  end
