open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Repr
open Ppx_deriving_schema.Deriving_helper

let build_assoc ~loc derive fs es =
  let fs =
    List.map2 fs es ~f:(fun (n, t) x ->
        let this = derive ~loc t x in
        { loc; txt = lident n }, this)
  in
  let record = pexp_record ~loc fs None in
  [%expr (Obj.magic [%mel.obj [%e record]] : Js.Json.t)]

let build_list' ~loc derive es ts = List.map2 ts es ~f:(derive ~loc)

let build_list ~loc derive es ts =
  [%expr
    (Obj.magic [%e pexp_array ~loc (build_list' ~loc derive es ts)]
      : Js.Json.t)]

let derive_of_tuple ~loc derive ts x =
  let n = List.length ts in
  let p, es = gen_pat_tuple ~loc "x" n in
  pexp_match ~loc x [ p --> build_list ~loc derive es ts ]

let derive_of_record ~loc derive fs x =
  let p, es = gen_pat_record ~loc "x" fs in
  pexp_match ~loc x [ p --> build_assoc ~loc derive fs es ]

let derive_of_variant ~loc derive cs x =
  let ctor_pat name pat =
    ppat_construct ~loc { loc; txt = lident name } pat
  in
  pexp_match ~loc x
    (List.map cs ~f:(function
      | Vc_record (n, fs) ->
          let p, es = gen_pat_record ~loc "x" fs in
          ctor_pat n (Some p)
          --> [%expr
                (Obj.magic
                   [|
                     string_to_json [%e estring ~loc n];
                     [%e build_assoc ~loc derive fs es];
                   |]
                  : Js.Json.t)]
      | Vc_tuple (n, ts) ->
          let arity = List.length ts in
          let p, es = gen_pat_tuple ~loc "x" arity in
          ctor_pat n (if arity = 0 then None else Some p)
          -->
          let es = build_list' ~loc derive es ts in
          let es = [%expr string_to_json [%e estring ~loc n]] :: es in
          [%expr (Obj.magic [%e pexp_array ~loc es] : Js.Json.t)]))

include Ppx_deriving_schema.Deriving1 (struct
  let name = "to_json"
  let t ~loc t = [%type: [%t t] -> Js.Json.t]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
end)

