open! ContainersLabels
open! Monomorphic
open Printf
open Ppxlib

type target = Target_native | Target_js

let mode = ref Target_native

let with_deriving ?(yojson_of = false) ?(of_yojson = false)
    type_declaration =
  let loc = type_declaration.ptype_loc in
  let payload =
    match yojson_of, of_yojson with
    | true, true -> [ [%stri yojson_of, of_yojson] ]
    | true, false -> [ [%stri yojson_of] ]
    | false, true -> [ [%stri of_yojson] ]
    | false, false -> []
  in
  let attr =
    {
      attr_loc = loc;
      attr_name = { txt = "deriving"; loc };
      attr_payload = PStr payload;
    }
  in
  { type_declaration with ptype_attributes = [ attr ] }

let longidentf ~loc fmt =
  ksprintf (fun txt -> { txt = Longident.parse txt; loc }) fmt

let estringf ~loc fmt = ksprintf (Ast_builder.Default.estring ~loc) fmt

let mod_decl_of_expr ~loc ~name ~expr =
  let open Ast_builder.Default in
  pstr_module ~loc
    (module_binding ~loc ~name:{ loc; txt = Some name.txt } ~expr)

let as_attr =
  Attribute.declare "remote.as" Attribute.Context.core_type
    Ast_pattern.(
      pstr
        (pstr_eval (pexp_constant (pconst_string __ drop drop)) nil
        ^:: nil))
    (fun x -> x)

let mutation_attr =
  Attribute.declare "remote.mutation" Attribute.Context.value_description
    Ast_pattern.(pstr nil)
    ()

let query_attr =
  Attribute.declare "remote.query" Attribute.Context.value_description
    Ast_pattern.(pstr nil)
    ()

module Method_desc = struct
  type t = {
    name : label loc;
    args : arg list;
    typ : core_type;
    kind : kind;
  }

  and kind = Kind_query | Kind_mutation

  and arg = {
    arg_label : arg_label;
    arg_name : label loc;
    arg_typ : core_type;
  }

  let of_value_description (desc : value_description) =
    let rec collect_args n args typ =
      let loc = typ.ptyp_loc in
      match typ.ptyp_desc with
      | Ptyp_arrow (arg_label, arg_typ, typ) ->
          let txt, n =
            match Attribute.get as_attr arg_typ with
            | Some txt -> txt, n
            | None -> (
                match arg_label with
                | Optional name | Labelled name -> name, n
                | Nolabel -> sprintf "param%i" n, n + 1)
          in
          let arg_name = { loc = arg_typ.ptyp_loc; txt } in
          let arg = { arg_label; arg_typ; arg_name } in
          collect_args n (arg :: args) typ
      | Ptyp_constr
          ({ txt = Ldot (Lident "Promise", "t"); loc = _ }, [ typ ]) -> (
          match List.is_empty args with
          | false -> Ok (List.rev args, typ)
          | true -> Error (loc, "remote: should be a function type"))
      | _ ->
          Error
            ( loc,
              "remote: the output type of an RPC method should be \
               Promise.t" )
    in
    let kind =
      match
        Attribute.get query_attr desc, Attribute.get mutation_attr desc
      with
      | None, None ->
          Error (desc.pval_loc, "specify either [@@mutation] or [@@query]")
      | Some (), Some () ->
          Error
            ( desc.pval_loc,
              "canont specify both [@@mutation] and [@@query]" )
      | Some (), None -> Ok Kind_query
      | None, Some () -> Ok Kind_mutation
    in
    match kind with
    | Error err -> Error err
    | Ok kind -> (
        match collect_args 0 [] desc.pval_type with
        | Error err -> Error err
        | Ok (args, typ) -> Ok { name = desc.pval_name; args; typ; kind })
end

let longident_unit = Longident.parse "unit"

let build_input_mod ~ctxt ?yojson_of ?of_yojson (m : Method_desc.t) =
  let open Ast_builder.Default in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let input_type =
    let fields =
      List.map m.args ~f:(fun (arg : Method_desc.arg) ->
          let type_, yojson_option =
            match arg.arg_label with
            | Nolabel | Labelled _ -> arg.arg_typ, false
            | Optional _ -> [%type: [%t arg.arg_typ] option], true
          in
          let drop_default_unit =
            match arg.arg_label, arg.arg_typ.ptyp_desc with
            | Nolabel, Ptyp_constr (ident, [])
              when Longident.compare ident.txt longident_unit = 0 ->
                true
            | _ -> false
          in
          let decl =
            label_declaration ~loc ~name:arg.arg_name ~mutable_:Immutable
              ~type_
          in
          let pld_attributes =
            let pld_attributes = ref [] in
            let add ?(payload = []) txt =
              let attr_name = { txt; loc } in
              pld_attributes :=
                { attr_loc = loc; attr_name; attr_payload = PStr payload }
                :: !pld_attributes
            in
            if yojson_option then add "yojson.option";
            if drop_default_unit then (
              add "yojson.default" ~payload:[%str ()];
              add "yojson.yojson_drop_default" ~payload:[%str ( = )]);
            !pld_attributes
          in
          { decl with pld_attributes })
    in
    let type_declaration =
      with_deriving ?yojson_of ?of_yojson
        (type_declaration ~loc ~name:{ txt = "t"; loc } ~manifest:None
           ~params:[] ~cstrs:[] ~private_:Public
           ~kind:(Ptype_record fields))
    in
    pstr_type ~loc Nonrecursive [ type_declaration ]
  in
  mod_decl_of_expr ~loc
    ~name:{ loc; txt = sprintf "Input_%s" m.name.txt }
    ~expr:
      (pmod_structure ~loc
         [%str
           (* open Ppx_yojson_conv_lib.Yojson_conv.Primitives *)
           [%%i input_type]])

let build_output_mod ~ctxt deriving_dir (m : Method_desc.t) =
  let open Ast_builder.Default in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let deriving_dir =
    match deriving_dir with
    | `yojson_of -> pexp_ident ~loc (longidentf ~loc "yojson_of")
    | `of_yojson -> pexp_ident ~loc (longidentf ~loc "of_yojson")
  in
  mod_decl_of_expr ~loc
    ~name:{ loc; txt = sprintf "Output_%s" m.name.txt }
    ~expr:
      (pmod_structure ~loc
         [%str
           (* open Ppx_yojson_conv_lib.Yojson_conv.Primitives *)
           type t = [%t m.typ] [@@deriving [%e deriving_dir]]])

let input_conv ~loc deriving_dir m =
  let deriving_dir =
    match deriving_dir with
    | `yojson_of -> "yojson_of_t"
    | `of_yojson -> "t_of_yojson"
  in
  Ast_builder.Default.(
    pexp_ident ~loc
      (longidentf ~loc "Input_%s.%s" m.Method_desc.name.txt deriving_dir))

let output_conv ~loc deriving_dir m =
  let deriving_dir =
    match deriving_dir with
    | `yojson_of -> "yojson_of_t"
    | `of_yojson -> "t_of_yojson"
  in
  Ast_builder.Default.(
    pexp_ident ~loc
      (longidentf ~loc "Output_%s.%s" m.Method_desc.name.txt deriving_dir))

let process_signature ~ctxt mod_type_decl f =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mod_type_decl.pmtd_type with
  | Some ({ pmty_desc = Pmty_signature items; _ } as mod_type) ->
      let methods =
        List.fold_left items ~init:[] ~f:(fun methods item ->
            match item.psig_desc with
            | Psig_value value_desc -> (
                match Method_desc.of_value_description value_desc with
                | Ok m -> Ok m :: methods
                | Error (loc, msg) ->
                    let msg = estringf ~loc "remote: %s" msg in
                    Error [%stri [%%ocaml.error [%e msg]]] :: methods)
            | _ ->
                let loc = item.psig_loc in
                Error
                  [%stri
                    [%%ocaml.error "remote: only values are allowed"]]
                :: methods)
      in
      f mod_type methods
  | _ -> [ [%stri [%ocaml.error "remote: is not a signature"]] ]

module Remote_browser = struct
  open Ast_builder.Default

  let build_remote_call ~ctxt ~path (m : Method_desc.t) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let rev_args = List.rev m.args in
    let define, make =
      match m.kind with
      | Kind_query ->
          [%expr Remote.define_query], [%expr Remote.make_query]
      | Kind_mutation ->
          [%expr Remote.define_mutation], [%expr Remote.make_mutation]
    in
    let input =
      pexp_record ~loc
        (List.map m.args ~f:(fun (a : Method_desc.arg) ->
             ( longidentf ~loc "Input_%s.%s" m.name.txt a.arg_name.txt,
               pexp_ident ~loc (longidentf ~loc "%s" a.arg_name.txt) )))
        None
    in
    let body =
      List.fold_left rev_args
        ~init:[%expr [%e make] query_def [%e input]]
        ~f:(fun prev (arg : Method_desc.arg) ->
          pexp_fun ~loc arg.arg_label None
            (ppat_var ~loc arg.arg_name)
            prev)
    in
    [%stri
      let [%p ppat_var ~loc m.name] =
        let query_def =
          [%e define]
            ~output_of_yojson:[%e output_conv `of_yojson ~loc m]
            ~yojson_of_input:[%e input_conv `yojson_of ~loc m]
            ~path:
              [%e
                estringf ~loc "%s/%s"
                  (Option.value path ~default:"")
                  m.name.txt]
        in
        [%e body]]

  let expand ~ctxt ~path mod_type_decl =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let str =
      process_signature ~ctxt mod_type_decl @@ fun _mod_type methods ->
      List.flat_map methods ~f:(function
        | Ok m ->
            [
              build_input_mod ~ctxt ~yojson_of:true m;
              build_output_mod `of_yojson ~ctxt m;
              build_remote_call ~ctxt ~path m;
            ]
        | Error str -> [ str ])
    in
    [
      pstr_module ~loc
        (module_binding ~loc
           ~name:{ loc; txt = Some mod_type_decl.pmtd_name.txt }
           ~expr:(pmod_structure ~loc str));
    ]
end

module Remote_native = struct
  open Ast_builder.Default

  let respond_error ~loc ?(payload = [%expr []]) ~status msg =
    [%expr
      let err : Yojson.Safe.t =
        `Assoc (("error", `String [%e estring ~loc msg]) :: [%e payload])
      in
      Dream.respond ~status:[%e status] (Yojson.Safe.to_string err)]

  let build_query ~loc ~path ~mod_name (m : Method_desc.t) =
    let define, make =
      match m.kind with
      | Kind_query ->
          [%expr Remote.define_query], [%expr Remote.make_query]
      | Kind_mutation ->
          [%expr Remote.define_mutation], [%expr Remote.make_mutation]
    in
    let make_query =
      (* Remote.make_query query_def {Input.arg1=...; arg2=...; ...} *)
      let body =
        let input =
          pexp_record ~loc
            (List.map m.args ~f:(fun (a : Method_desc.arg) ->
                 ( longidentf ~loc "Input_%s.%s" m.name.txt a.arg_name.txt,
                   pexp_ident ~loc (longidentf ~loc "%s" a.arg_name.txt) )))
            None
        in
        [%expr [%e make] query_def [%e input]]
      in
      (* fun arg1 arg2 -> ... *)
      List.fold_left (List.rev m.args) ~init:body
        ~f:(fun prev (arg : Method_desc.arg) ->
          pexp_fun ~loc arg.arg_label None
            (ppat_var ~loc arg.arg_name)
            prev)
    in
    (* fun input -> f input.arg1 input.arg2 ... *)
    let call_into_impl =
      let app =
        pexp_apply ~loc
          (pexp_ident ~loc
             (longidentf ~loc "%s.%s" mod_name.txt m.name.txt))
          (List.map m.args
             ~f:(fun { Method_desc.arg_label; arg_name; arg_typ = _ } ->
               let arg =
                 pexp_field ~loc [%expr input]
                   (longidentf ~loc "Input_%s.%s" m.name.txt arg_name.txt)
               in
               arg_label, arg))
      in
      [%expr fun input -> [%e app]]
    in
    [%stri
      let [%p ppat_var ~loc m.name] =
        let query_def =
          [%e define]
            ~yojson_of_output:[%e output_conv `yojson_of ~loc m]
            ~yojson_of_input:[%e input_conv `yojson_of ~loc m]
            ~path:
              [%e
                estringf ~loc "%s/%s"
                  (Option.value path ~default:"")
                  m.name.txt]
            [%e call_into_impl]
        in
        [%e make_query]]

  let build_functor ~ctxt ~path ~mod_name (m : Method_desc.t) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let body_req =
      let call_into_impl =
        let f =
          pexp_ident ~loc
            (longidentf ~loc "%s.%s" mod_name.txt m.name.txt)
        in
        pexp_apply ~loc f
          (List.map m.args ~f:(fun (arg : Method_desc.arg) ->
               ( arg.arg_label,
                 pexp_field ~loc [%expr input]
                   (longidentf ~loc "%s" arg.arg_name.txt) )))
      in
      let json_data =
        match m.kind with
        | Kind_query ->
            [%expr
              match Dream.query req "input" with
              | Some data -> Lwt.return data
              | None -> Lwt.return "{}"]
        | Kind_mutation -> [%expr Dream.body req]
      in
      [%expr
        let open Lwt.Infix in
        [%e json_data] >>= fun json_data ->
        match Yojson.Safe.from_string json_data with
        | exception Yojson.Json_error _ ->
            [%e
              respond_error ~loc ~status:[%expr `Bad_Request]
                "invalid JSON"]
        | json -> (
            match [%e input_conv `of_yojson ~loc m] json with
            | exception
                Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json)
              ->
                print_endline (Printexc.to_string exn);
                [%e
                  respond_error ~loc ~status:[%expr `Bad_Request]
                    ~payload:[%expr [ "json", json ]]
                    "invalid JSON payload"]
            | input ->
                [%e call_into_impl] >>= fun output ->
                let json = [%e output_conv `yojson_of ~loc m] output in
                let data = Yojson.Safe.to_string json in
                Dream.respond ~status:`OK data)]
    in
    [%str
      [%%i build_input_mod ~ctxt ~yojson_of:true ~of_yojson:true m]
      [%%i build_output_mod `yojson_of ~ctxt m]
      [%%i build_query ~loc ~path ~mod_name m]

      let [%p
            ppat_var ~loc
              {
                m.Method_desc.name with
                txt = sprintf "%s_req" m.name.txt;
              }] =
       fun req -> [%e body_req]]

  let expand ~ctxt ~path mod_type_decl =
    process_signature ~ctxt mod_type_decl @@ fun mod_type methods ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let mod_name = mod_type_decl.pmtd_name in
    let make_str =
      List.flat_map methods ~f:(function
        | Ok m -> build_functor ~ctxt ~path ~mod_name m
        | Error str -> [ str ])
    in
    let routes_expr =
      List.fold_left (List.rev methods) ~init:[%expr []]
        ~f:(fun routes m ->
          match m with
          | Ok m ->
              let register_route =
                match m.Method_desc.kind with
                | Kind_query -> [%expr Dream.get]
                | Kind_mutation -> [%expr Dream.post]
              in
              let path =
                let path =
                  match path with
                  | None -> m.name.txt
                  | Some path -> sprintf "%s/%s" path m.name.txt
                in
                estring ~loc path
              in
              let route =
                [%expr
                  [%e register_route] [%e path]
                    [%e
                      pexp_ident ~loc
                        (longidentf ~loc "%s_req" m.name.txt)]]
              in
              let route_not_allowed =
                [%expr
                  Dream.any [%e path] (fun _req ->
                      [%e
                        respond_error ~loc
                          ~status:[%expr `Method_Not_Allowed]
                          "method not allowed"])]
              in
              [%expr [%e route] :: [%e route_not_allowed] :: [%e routes]]
          | Error _ -> routes)
    in
    let make_str = make_str @ [%str let routes = [%e routes_expr]] in
    let impl_mod_make =
      mod_decl_of_expr ~loc
        ~name:{ mod_name with txt = sprintf "%s_make" mod_name.txt }
        ~expr:
          (pmod_functor ~loc
             (Named ({ txt = Some mod_name.txt; loc }, mod_type))
             (pmod_structure ~loc make_str))
    in
    [ impl_mod_make ]
end

let derive_remote ~ctxt modtype path =
  match !mode with
  | Target_native -> Remote_native.expand ~ctxt ~path modtype
  | Target_js -> Remote_browser.expand ~ctxt ~path modtype

let _ =
  let args = Deriving.Args.(empty +> arg "path" (estring __)) in
  let str_module_type_decl =
    Deriving.Generator.V2.make args derive_remote
  in
  Deriving.add ~str_module_type_decl "remote"

let () =
  Driver.add_arg "-js"
    (Unit (fun () -> mode := Target_js))
    ~doc:"preprocess for JS build"
