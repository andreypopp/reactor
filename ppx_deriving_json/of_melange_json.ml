open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Repr
open Ppx_deriving_schema.Deriving_helper

let build_tuple ~loc derive si ts e =
  pexp_tuple ~loc
    (List.mapi ts ~f:(fun i t ->
         derive ~loc t
           [%expr Js.Array.unsafe_get [%e e] [%e eint ~loc (si + i)]]))

let build_js_type ~loc fs =
  let f (id, _) =
    let pof_desc = Otag (id, [%type: Js.Json.t Js.undefined]) in
    { pof_loc = loc; pof_attributes = []; pof_desc }
  in
  let row = ptyp_object ~loc (List.map fs ~f) Closed in
  [%type: [%t row] Js.t]

let build_record ~loc derive fs x =
  let handle_field fs (n, t) =
    ( to_lident n,
      [%expr
        match
          Js.Undefined.toOption
            [%e fs] ## [%e pexp_ident ~loc:n.loc (to_lident n)]
        with
        | Stdlib.Option.Some v -> [%e derive ~loc t [%expr v]]
        | Stdlib.Option.None ->
            Json.of_json_error
              [%e estring ~loc (sprintf "missing field %S" n.txt)]] )
  in
  [%expr
    let fs = (Obj.magic [%e x] : [%t build_js_type ~loc fs]) in
    [%e pexp_record ~loc (List.map fs ~f:(handle_field [%expr fs])) None]]

let derive_of_tuple ~loc derive ts x =
  let n = List.length ts in
  [%expr
    if
      Js.Array.isArray [%e x]
      && Js.Array.length (Obj.magic [%e x] : Js.Json.t array)
         = [%e eint ~loc n]
    then
      let es = (Obj.magic [%e x] : Js.Json.t array) in
      [%e build_tuple ~loc derive 0 ts [%expr es]]
    else
      Json.of_json_error
        [%e estring ~loc (sprintf "expected a JSON array of length %i" n)]]

let eis_json_object ~loc x =
  [%expr
    Js.typeof [%e x] = "object"
    && (not (Js.Array.isArray [%e x]))
    && not ((Obj.magic [%e x] : 'a Js.null) == Js.null)]

let ensure_json_object ~loc x =
  [%expr
    if not [%e eis_json_object ~loc x] then
      Json.of_json_error
        [%e estring ~loc (sprintf "expected a JSON object")]]

let ensure_json_array_len ~loc n len =
  [%expr
    if [%e len] <> [%e eint ~loc n] then
      Json.of_json_error
        [%e estring ~loc (sprintf "expected a JSON array of length %i" n)]]

let derive_of_record ~loc derive fs x =
  [%expr
    [%e ensure_json_object ~loc x];
    [%e build_record ~loc derive fs x]]

let derive_of_variant ~loc derive cs x =
  let fail_case = [%expr Json.of_json_error "invalid JSON"] in
  let cases =
    let econstruct (n : label loc) arg =
      pexp_construct ~loc:n.loc (to_lident n) arg
    in
    List.fold_left (List.rev cs) ~init:fail_case ~f:(fun next c ->
        match c with
        | Vc_record (n, fs) ->
            [%expr
              if tag = [%e estring ~loc:n.loc n.txt] then (
                [%e ensure_json_array_len ~loc 2 [%expr len]];
                let fs = Js.Array.unsafe_get array 1 in
                [%e ensure_json_object ~loc [%expr fs]];
                [%e
                  econstruct n
                    (Some (build_record ~loc derive fs [%expr fs]))])
              else [%e next]]
        | Vc_tuple (n, ts) ->
            let arity = List.length ts in
            [%expr
              if tag = [%e estring ~loc:n.loc n.txt] then (
                [%e ensure_json_array_len ~loc (arity + 1) [%expr len]];
                [%e
                  if arity = 0 then econstruct n None
                  else
                    econstruct n
                      (Some (build_tuple ~loc derive 1 ts [%expr e]))])
              else [%e next]])
  in
  [%expr
    if Js.Array.isArray [%e x] then
      let array = (Obj.magic [%e x] : Js.Json.t array) in
      let len = Js.Array.length array in
      if len > 0 then
        let tag = Js.Array.unsafe_get array 0 in
        if Js.typeof tag = "string" then
          let tag = (Obj.magic tag : string) in
          [%e cases]
        else
          Json.of_json_error
            "expected a non empty JSON array with element being a string"
      else Json.of_json_error "expected a non empty JSON array"
    else Json.of_json_error "expected a non empty JSON array"]

include Ppx_deriving_schema.Deriving1 (struct
  let name = "of_json"
  let t ~loc t = [%type: Js.Json.t -> [%t t]]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
  let derive_of_polyvariant ~loc:_ _ = failwith "TODO"
end)
