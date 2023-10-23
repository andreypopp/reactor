open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Trepr.Repr
open Trepr.Deriving_helper

let build_tuple ~loc derive es ts =
  let args =
    List.fold_left
      (List.rev (List.combine es ts))
      ~init:[]
      ~f:(fun prev (x, t) ->
        let this = derive ~loc t x in
        this :: prev)
  in
  pexp_tuple ~loc args

let build_record ~loc derive fs x =
  with_refs ~loc "x" fs @@ fun ename ->
  let handle_field k v =
    let fail_case =
      [%pat? name]
      --> [%expr
            Json.of_json_error
              (Stdlib.Printf.sprintf "unknown field: %s" name)]
    in
    let cases =
      List.fold_left (List.rev fs) ~init:[ fail_case ]
        ~f:(fun next (n, t) ->
          pstring ~loc n
          --> [%expr
                [%e ename n] := Stdlib.Option.Some [%e derive ~loc t v]]
          :: next)
    in
    pexp_match ~loc k cases
  in
  let build =
    let fields =
      List.map fs ~f:(fun (n, _) ->
          ( { loc; txt = lident n },
            [%expr
              match Stdlib.( ! ) [%e ename n] with
              | Stdlib.Option.Some v -> v
              | Stdlib.Option.None ->
                  Json.of_json_error
                    [%e estring ~loc (sprintf "missing field %S" n)]] ))
    in
    pexp_record ~loc fields None
  in
  [%expr
    let rec iter = function
      | [] -> ()
      | (n', v) :: fs ->
          [%e handle_field [%expr n'] [%expr v]];
          iter fs
    in
    iter [%e x];
    [%e build]]

let derive_of_tuple ~loc derive ts x =
  let n = List.length ts in
  let xpatt, xexprs = gen_pat_list ~loc "x" n in
  let xpatt = [%pat? `List [%p xpatt]] in
  pexp_match ~loc x
    [
      xpatt --> build_tuple ~loc derive xexprs ts;
      [%pat? _]
      --> [%expr
            Json.of_json_error
              [%e
                estring ~loc
                  (sprintf "expected a JSON array of length %i" n)]];
    ]

let derive_of_record ~loc derive fs x =
  pexp_match ~loc x
    [
      [%pat? `Assoc fs] --> build_record ~loc derive fs [%expr fs];
      [%pat? _]
      --> [%expr
            Json.of_json_error
              [%e estring ~loc (sprintf "expected a JSON object")]];
    ]

let derive_of_variant ~loc derive cs x =
  let fail_case =
    [%pat? _] --> [%expr Json.of_json_error "invalid JSON"]
  in
  let cases =
    let econstruct name arg =
      pexp_construct ~loc { loc; txt = lident name } arg
    in
    List.fold_left (List.rev cs) ~init:[ fail_case ] ~f:(fun next c ->
        let case =
          match c with
          | Vc_record (name, fs) ->
              [%pat? `List [ `String [%p pstring ~loc name]; `Assoc fs ]]
              --> econstruct name
                    (Some (build_record ~loc derive fs [%expr fs]))
          | Vc_tuple (name, ts) ->
              let n = List.length ts in
              if n = 0 then
                [%pat? `List [ `String [%p pstring ~loc name] ]]
                --> econstruct name None
              else
                let xpatt, xexprs = gen_pat_list ~loc "x" n in
                [%pat?
                  `List (`String [%p pstring ~loc name] :: [%p xpatt])]
                --> econstruct name
                      (Some (build_tuple ~loc derive xexprs ts))
        in
        case :: next)
  in
  pexp_match ~loc x cases

include Trepr.Deriving1 (struct
  let name = "of_json"
  let t ~loc t = [%type: Yojson.Basic.t -> [%t t]]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
end)

let () = register ()
