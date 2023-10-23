open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Repr
open Ppx_deriving_schema.Deriving_helper

let with_refs ~loc prefix fs inner =
  let gen_name n = sprintf "%s_%s" prefix n in
  let gen_expr (n : label loc) =
    pexp_ident ~loc:n.loc { loc = n.loc; txt = lident (gen_name n.txt) }
  in
  List.fold_left (List.rev fs) ~init:(inner gen_expr)
    ~f:(fun next ((n : label loc), _t) ->
      let patt =
        ppat_var ~loc:n.loc { loc = n.loc; txt = gen_name n.txt }
      in
      [%expr
        let [%p patt] = ref Stdlib.Option.None in
        [%e next]])

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
        ~f:(fun next ((n : label loc), t) ->
          pstring ~loc:n.loc n.txt
          --> [%expr
                [%e ename n] := Stdlib.Option.Some [%e derive ~loc t v]]
          :: next)
    in
    pexp_match ~loc k cases
  in
  let build =
    let fields =
      List.map fs ~f:(fun (n, _) ->
          ( to_lident n,
            [%expr
              match Stdlib.( ! ) [%e ename n] with
              | Stdlib.Option.Some v -> v
              | Stdlib.Option.None ->
                  Json.of_json_error
                    [%e
                      estring ~loc:n.loc
                        (sprintf "missing field %S" n.txt)]] ))
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
    let econstruct n arg = pexp_construct ~loc (to_lident n) arg in
    List.fold_left (List.rev cs) ~init:[ fail_case ] ~f:(fun next c ->
        let case =
          match c with
          | Vc_record (n, fs) ->
              [%pat?
                `List [ `String [%p pstring ~loc:n.loc n.txt]; `Assoc fs ]]
              --> econstruct n
                    (Some (build_record ~loc derive fs [%expr fs]))
          | Vc_tuple (n, ts) ->
              let arity = List.length ts in
              if arity = 0 then
                [%pat? `List [ `String [%p pstring ~loc:n.loc n.txt] ]]
                --> econstruct n None
              else
                let xpatt, xexprs = gen_pat_list ~loc "x" arity in
                [%pat?
                  `List
                    (`String [%p pstring ~loc:n.loc n.txt] :: [%p xpatt])]
                --> econstruct n
                      (Some (build_tuple ~loc derive xexprs ts))
        in
        case :: next)
  in
  pexp_match ~loc x cases

include Ppx_deriving_schema.Deriving1 (struct
  let name = "of_json"
  let t ~loc t = [%type: Yojson.Basic.t -> [%t t]]
  let derive_of_tuple = derive_of_tuple
  let derive_of_record = derive_of_record
  let derive_of_variant = derive_of_variant
end)
