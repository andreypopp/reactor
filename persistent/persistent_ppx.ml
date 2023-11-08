open Ppxlib
open Ast_builder.Default
open ContainersLabels
open Ppx_deriving_schema
open Deriving_helper

let derive_decode =
  object (self)
    inherit deriving1
    method name = "decode"

    method t ~loc t =
      [%type: Persistent.ctx * Sqlite3.Data.t array -> [%t t]]

    method! derive_of_tuple ~loc ts x =
      let n = List.length ts in
      let ps, e = gen_tuple ~loc "x" n in
      List.fold_left2 (List.rev ps) (List.rev ts) ~init:e
        ~f:(fun next p t ->
          [%expr
            let [%p p] = [%e self#derive_of_type_expr ~loc t x] in
            [%e next]])

    method! derive_of_record ~loc fs x =
      let ps, e = gen_record ~loc "x" fs in
      List.fold_left2 (List.rev ps) (List.rev fs) ~init:e
        ~f:(fun next p (_, t) ->
          [%expr
            let [%p p] = [%e self#derive_of_type_expr ~loc t x] in
            [%e next]])
  end

let derive_bind =
  object (self)
    inherit deriving1
    method name = "bind"

    method t ~loc t =
      [%type: [%t t] -> Persistent.ctx -> Sqlite3.stmt -> unit]

    method! derive_of_tuple ~loc ts x =
      let n = List.length ts in
      let p, es = gen_pat_tuple ~loc "x" n in
      let e =
        List.fold_left2 (List.rev es) (List.rev ts) ~init:[%expr ()]
          ~f:(fun next e t ->
            [%expr
              [%e self#derive_of_type_expr ~loc t e] ctx stmt;
              [%e next]])
      in
      [%expr fun ctx stmt -> [%e pexp_match ~loc x [ p --> e ]]]

    method! derive_of_record ~loc fs x =
      let p, es = gen_pat_record ~loc "x" fs in
      let e =
        List.fold_left2 (List.rev es) (List.rev fs) ~init:[%expr ()]
          ~f:(fun next e (_, t) ->
            [%expr
              [%e self#derive_of_type_expr ~loc t e] ctx stmt;
              [%e next]])
      in
      [%expr fun ctx stmt -> [%e pexp_match ~loc x [ p --> e ]]]
  end

let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]])

let derive_columns =
  object (self)
    inherit deriving1
    method name = "columns"
    method t ~loc _t = [%type: string -> (string * string) list]

    method! derive_of_tuple ~loc ts x =
      let es =
        List.mapi ts ~f:(fun i t ->
            let i = eint ~loc i in
            [%expr
              [%e self#derive_of_type_expr ~loc t [%expr genname [%e i]]]])
      in
      [%expr
        let genname =
          match [%e x] with
          | "" -> fun i -> Printf.sprintf "c%i" i
          | prefix -> fun i -> Printf.sprintf "%s_%i" prefix i
        in
        List.flatten [%e pexp_list ~loc es]]

    method! derive_of_record ~loc fs x =
      let es =
        List.map fs ~f:(fun ((n : label loc), t) ->
            let n = estring ~loc:n.loc n.txt in
            [%expr
              [%e self#derive_of_type_expr ~loc t [%expr genname [%e n]]]])
      in
      [%expr
        let genname =
          match [%e x] with
          | "" -> Fun.id
          | prefix -> fun n -> Printf.sprintf "%s_%s" prefix n
        in
        List.flatten [%e pexp_list ~loc es]]
  end

let codec =
  Deriving.add "codec"
    ~str_type_decl:
      (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt str ->
           derive_decode#generator ~ctxt str
           @ derive_bind#generator ~ctxt str
           @ derive_columns#generator ~ctxt str))

let _ =
  let derive_table ({ name; params; shape = _; loc } : Repr.type_decl) =
    if not (List.is_empty params) then not_supported "type parameters";
    let pat = ppat_var ~loc name in
    let columns = name_loc_of_t "columns" name in
    let bind = lident_of_t "bind" name in
    let decode = lident_of_t "decode" name in
    let columns =
      { loc = columns.loc; txt = Longident.parse columns.txt }
    in
    value_binding ~loc ~pat
      ~expr:
        [%expr
          {
            Persistent.table = [%e estring ~loc name.txt];
            columns = [%e pexp_ident ~loc columns] "";
            decode = [%e pexp_ident ~loc decode];
            bind = [%e pexp_ident ~loc bind];
          }]
  in
  Deriving.add "entity"
    ~str_type_decl:
      (Deriving.Generator.V2.make ~deps:[ codec ] Deriving.Args.empty
         (fun ~ctxt (rec_flag, type_decls) ->
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           match List.map type_decls ~f:Repr.of_type_declaration with
           | exception Not_supported msg ->
               [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
           | reprs -> (
               try
                 let bindings = List.map reprs ~f:derive_table in
                 [%str
                   [@@@ocaml.warning "-39-11"]

                   [%%i pstr_value ~loc rec_flag bindings]]
               with Not_supported msg ->
                 [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ])))
