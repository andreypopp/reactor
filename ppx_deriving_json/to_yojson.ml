open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Repr
open Ppx_deriving_schema.Deriving_helper

let build_assoc ~loc derive fs es =
  let expr =
    List.fold_left
      (List.rev (List.combine fs es))
      ~init:[%expr []]
      ~f:(fun prev (((n : label loc), t), x) ->
        let this = derive ~loc t x in
        [%expr ([%e estring ~loc:n.loc n.txt], [%e this]) :: [%e prev]])
  in
  [%expr `Assoc [%e expr]]

let build_list' ~loc derive es ts =
  List.fold_left
    (List.rev (List.combine es ts))
    ~init:[%expr []]
    ~f:(fun prev (x, t) ->
      let this = derive ~loc t x in
      [%expr [%e this] :: [%e prev]])

let build_list ~loc derive es ts =
  let expr = build_list' ~loc derive es ts in
  [%expr `List [%e expr]]

let derive_of_tuple ~loc derive ts x =
  let n = List.length ts in
  let p, es = gen_pat_tuple ~loc "x" n in
  pexp_match ~loc x [ p --> build_list ~loc derive es ts ]

let derive_of_record ~loc derive fs x =
  let p, es = gen_pat_record ~loc "x" fs in
  pexp_match ~loc x [ p --> build_assoc ~loc derive fs es ]

let derive_of_variant ~loc derive cs x =
  let ctor_pat name pat = ppat_construct ~loc (to_lident name) pat in
  pexp_match ~loc x
    (List.map cs ~f:(function
      | Vc_record (n, fs) ->
          let p, es = gen_pat_record ~loc "x" fs in
          ctor_pat n (Some p)
          --> [%expr
                `List
                  [
                    `String [%e estring ~loc:n.loc n.txt];
                    [%e build_assoc ~loc derive fs es];
                  ]]
      | Vc_tuple (n, ts) ->
          let arity = List.length ts in
          let p, es = gen_pat_tuple ~loc "x" arity in
          ctor_pat n (if arity = 0 then None else Some p)
          --> [%expr
                `List
                  (`String [%e estring ~loc:n.loc n.txt]
                  :: [%e build_list' ~loc derive es ts])]))

include Ppx_deriving_schema.Deriving1 (struct
  let name = "to_json"
  let t ~loc t = [%type: [%t t] -> Yojson.Basic.t]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
end)
