open Printf
open ContainersLabels
open Ppxlib
open Ast_builder.Default
open Ppx_deriving_schema.Deriving_helper

module Of_json = struct
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

  let build_record ~loc derive fs x make =
    with_refs ~loc "x" fs @@ fun ename ->
    let handle_field k v =
      let fail_case =
        [%pat? name]
        --> [%expr
              Ppx_deriving_json_runtime.of_json_error
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
                    Ppx_deriving_json_runtime.of_json_error
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
      [%e make build]]

  let derive_of_tuple ~loc derive ts x =
    let n = List.length ts in
    let xpatt, xexprs = gen_pat_list ~loc "x" n in
    let xpatt = [%pat? `List [%p xpatt]] in
    pexp_match ~loc x
      [
        xpatt --> build_tuple ~loc derive xexprs ts;
        [%pat? _]
        --> [%expr
              Ppx_deriving_json_runtime.of_json_error
                [%e
                  estring ~loc
                    (sprintf "expected a JSON array of length %i" n)]];
      ]

  let derive_of_record ~loc derive fs x =
    pexp_match ~loc x
      [
        [%pat? `Assoc fs]
        --> build_record ~loc derive fs [%expr fs] Fun.id;
        [%pat? _]
        --> [%expr
              Ppx_deriving_json_runtime.of_json_error
                [%e estring ~loc (sprintf "expected a JSON object")]];
      ]

  let derive_of_variant_case ~loc derive make n ts =
    let arity = List.length ts in
    if arity = 0 then
      [%pat? `List [ `String [%p pstring ~loc:n.loc n.txt] ]]
      --> make None
    else
      let xpatt, xexprs = gen_pat_list ~loc "x" arity in
      [%pat? `List (`String [%p pstring ~loc:n.loc n.txt] :: [%p xpatt])]
      --> make (Some (build_tuple ~loc derive xexprs ts))

  let derive_of_variant_case_record ~loc derive make n fs =
    [%pat? `List [ `String [%p pstring ~loc:n.loc n.txt]; `Assoc fs ]]
    --> build_record ~loc derive fs [%expr fs] (fun e -> make (Some e))

  let deriving =
    Ppx_deriving_schema.deriving_of_match () ~name:"of_json"
      ~of_t:(fun ~loc -> [%type: Yojson.Basic.t])
      ~error:(fun ~loc ->
        [%expr Ppx_deriving_json_runtime.of_json_error "invalid JSON"])
      ~derive_of_tuple ~derive_of_record ~derive_of_variant_case
      ~derive_of_variant_case_record
end

module To_json = struct
  let pexp_list ~loc xs =
    List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
        [%expr [%e x] :: [%e xs]])

  let derive_of_tuple ~loc derive ts es =
    let es = List.map2 ts es ~f:(derive ~loc) in
    [%expr `List [%e pexp_list ~loc es]]

  let derive_of_record ~loc derive fs es =
    let es =
      List.map2 fs es ~f:(fun (n, t) x ->
          [%expr [%e estring ~loc:n.loc n.txt], [%e derive ~loc t x]])
    in
    [%expr `Assoc [%e pexp_list ~loc es]]

  let derive_of_variant_case ~loc derive n ts es =
    [%expr
      `List
        (`String [%e estring ~loc:n.loc n.txt]
        :: [%e pexp_list ~loc (List.map2 ts es ~f:(derive ~loc))])]

  let derive_of_variant_case_record ~loc derive n fs es =
    [%expr
      `List
        (`String [%e estring ~loc:n.loc n.txt]
        :: [ [%e derive_of_record ~loc derive fs es] ])]

  let deriving =
    Ppx_deriving_schema.deriving_to () ~name:"to_json"
      ~t_to:(fun ~loc -> [%type: Yojson.Basic.t])
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
