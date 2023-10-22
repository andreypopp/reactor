open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Trepr.Repr
open Trepr.Deriving_helper

let build_tuple ~loc derive si ts e =
  let args =
    List.mapi ts ~f:(fun i t ->
        let x =
          [%expr Js.Array.unsafe_get [%e e] [%e eint ~loc (si + i)]]
        in
        derive ~loc t x)
  in
  pexp_tuple ~loc args

let build_record ~loc derive ns ts fs =
  let rowtyp =
    ptyp_object ~loc
      (List.map ns ~f:(fun n ->
           {
             pof_loc = loc;
             pof_attributes = [];
             pof_desc =
               Otag ({ loc; txt = n }, [%type: Js.Json.t Js.undefined]);
           }))
      Closed
  in
  [%expr
    let fs = (Obj.magic fs : [%t rowtyp] Js.t) in
    [%e
      let fs =
        List.map2 ns ts ~f:(fun n t ->
            ( { loc; txt = lident n },
              [%expr
                match
                  Js.Undefined.toOption [%e fs] ## [%e estring ~loc n]
                with
                | Stdlib.Option.Some v -> [%e derive ~loc t [%expr v]]
                | Stdlib.Option.None ->
                    Json.of_json_error
                      [%e estring ~loc (sprintf "missing field %S" n)]] ))
      in
      pexp_record ~loc fs None]]

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

let derive_of_record ~loc derive fs x =
  let ns = List.map fs ~f:fst in
  let ts = List.map fs ~f:snd in
  [%expr
    if [%e eis_json_object ~loc x] then
      let fs = (Obj.magic [%e x] : Js.Json.t Js.Dict.t) in
      [%e build_record ~loc derive ns ts [%expr fs]]
    else
      Json.of_json_error
        [%e estring ~loc (sprintf "expected a JSON object")]]

let derive_of_variant ~loc derive cs x =
  let fail_case = [%expr Json.of_json_error "invalid JSON"] in
  let cases =
    let econstruct name arg =
      pexp_construct ~loc { loc; txt = lident name } arg
    in
    List.fold_left (List.rev cs) ~init:fail_case ~f:(fun next c ->
        match c with
        | Vc_record (name, fs) ->
            [%expr
              if tag = [%e estring ~loc name] then
                if len <> 2 then
                  Json.of_json_error "expected a JSON array of length 2"
                else
                  let fs = Js.Array.unsafe_get array 1 in
                  if not [%e eis_json_object ~loc [%expr fs]] then
                    Json.of_json_error "expected a JSON object"
                  else
                    let fs = (Obj.magic fs : Js.Json.t Js.Dict.t) in
                    [%e
                      let ns = List.map fs ~f:fst in
                      let ts = List.map fs ~f:snd in
                      econstruct name
                        (Some (build_record ~loc derive ns ts [%expr fs]))]
              else [%e next]]
        | Vc_tuple (name, ts) ->
            let n = List.length ts in
            let n1 = n + 1 in
            [%expr
              if tag = [%e estring ~loc name] then
                if len <> [%e eint ~loc n1] then
                  Json.of_json_error
                    [%e
                      estring ~loc
                        (sprintf "expected a JSON array of length %i" n1)]
                else
                  [%e
                    if n = 0 then econstruct name None
                    else
                      econstruct name
                        (Some (build_tuple ~loc derive 1 ts [%expr e]))]
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

include Trepr.Deriving1 (struct
  let name = "of_json"
  let t ~loc t = [%type: Js.Json.t -> [%t t]]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
end)

let () = register ()
