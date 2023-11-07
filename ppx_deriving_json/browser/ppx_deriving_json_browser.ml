open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Deriving_helper

module Of_json = struct
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

  let build_record ~loc derive fs x make =
    let handle_field fs (n, t) =
      ( to_lident n,
        [%expr
          match
            Js.Undefined.toOption
              [%e fs] ## [%e pexp_ident ~loc:n.loc (to_lident n)]
          with
          | Stdlib.Option.Some v -> [%e derive ~loc t [%expr v]]
          | Stdlib.Option.None ->
              Ppx_deriving_json_runtime.of_json_error
                [%e estring ~loc (sprintf "missing field %S" n.txt)]] )
    in
    [%expr
      let fs = (Obj.magic [%e x] : [%t build_js_type ~loc fs]) in
      [%e
        make
          (pexp_record ~loc
             (List.map fs ~f:(handle_field [%expr fs]))
             None)]]

  let eis_json_object ~loc x =
    [%expr
      Js.typeof [%e x] = "object"
      && (not (Js.Array.isArray [%e x]))
      && not ((Obj.magic [%e x] : 'a Js.null) == Js.null)]

  let ensure_json_object ~loc x =
    [%expr
      if not [%e eis_json_object ~loc x] then
        Ppx_deriving_json_runtime.of_json_error
          [%e estring ~loc (sprintf "expected a JSON object")]]

  let ensure_json_array_len ~loc n len =
    [%expr
      if [%e len] <> [%e eint ~loc n] then
        Ppx_deriving_json_runtime.of_json_error
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

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
        Ppx_deriving_json_runtime.of_json_error
          [%e
            estring ~loc (sprintf "expected a JSON array of length %i" n)]]

  let derive_of_record ~loc derive fs x =
    [%expr
      [%e ensure_json_object ~loc x];
      [%e build_record ~loc derive fs x Fun.id]]

  let derive_of_variant ~loc _derive cases x =
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
            Ppx_deriving_json_runtime.of_json_error
              "expected a non empty JSON array with element being a \
               string"
        else
          Ppx_deriving_json_runtime.of_json_error
            "expected a non empty JSON array"
      else
        Ppx_deriving_json_runtime.of_json_error
          "expected a non empty JSON array"]

  let derive_of_variant_case ~loc derive make (n : label loc) ts next =
    let arity = List.length ts in
    [%expr
      if tag = [%e estring ~loc:n.loc n.txt] then (
        [%e ensure_json_array_len ~loc (arity + 1) [%expr len]];
        [%e
          if arity = 0 then make None
          else make (Some (build_tuple ~loc derive 1 ts [%expr array]))])
      else [%e next]]

  let derive_of_variant_case_record ~loc derive make (n : label loc) fs
      next =
    [%expr
      if tag = [%e estring ~loc:n.loc n.txt] then (
        [%e ensure_json_array_len ~loc 2 [%expr len]];
        let fs = Js.Array.unsafe_get array 1 in
        [%e ensure_json_object ~loc [%expr fs]];
        [%e
          build_record ~loc derive fs [%expr fs] (fun e -> make (Some e))])
      else [%e next]]

  let deriving =
    Ppx_deriving_schema.deriving_of () ~name:"of_json"
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_json_runtime.of_json_error "invalid JSON"])
      ~of_t:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant
      ~derive_of_variant_case ~derive_of_variant_case_record
end

module To_json = struct
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

  let deriving =
    Ppx_deriving_schema.deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Js.Json.t])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
      ~derive_of_variant_case_record
end

let () =
  let _ = Ppx_deriving_schema.register Of_json.deriving in
  let _ = Ppx_deriving_schema.register To_json.deriving in
  let _ =
    Ppx_deriving_schema.(
      register (combined ~name:"json" Of_json.deriving To_json.deriving))
  in
  ()
