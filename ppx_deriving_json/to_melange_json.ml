open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Deriving_helper

let as_json ~loc x = [%expr (Obj.magic [%e x] : Js.Json.t)]

let derive_of_tuple ~loc derive ts es =
  as_json ~loc (pexp_array ~loc (List.map2 ts es ~f:(derive ~loc)))

let derive_of_record ~loc derive fs es =
  let fs =
    List.map2 fs es ~f:(fun (n, t) x ->
        let this = derive ~loc t x in
        to_lident n, this)
  in
  let record = pexp_record ~loc fs None in
  as_json ~loc [%expr [%mel.obj [%e record]]]

let derive_of_variant_case ~loc derive n ts es =
  let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
  let es = List.map2 ts es ~f:(derive ~loc) in
  as_json ~loc (pexp_array ~loc (tag :: es))

let derive_of_variant_case_record ~loc derive n fs es =
  let tag = [%expr string_to_json [%e estring ~loc:n.loc n.txt]] in
  let es = [ derive_of_record ~loc derive fs es ] in
  as_json ~loc (pexp_array ~loc (tag :: es))

let to_json =
  Ppx_deriving_schema.deriving_to () ~name:"to_json"
    ~t_to:(fun ~loc -> [%type: Js.Json.t])
    ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
    ~derive_of_variant_case_record
