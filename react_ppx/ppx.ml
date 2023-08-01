open Ppxlib

type mode = Export | Melange

let mode = ref Melange

let longident label =
  { txt = Longident.Lident label.txt; loc = label.loc }

let ident label =
  let open Ast_builder.Default in
  pexp_ident ~loc:label.loc (longident label)

module Ext_component = struct
  let expand ~ctxt pat expr =
    let open Ast_builder.Default in
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let label =
      match pat.ppat_desc with
      | Ppat_var label -> label
      | _ -> failwith "not expected"
    in
    let args =
      let rec collect_props acc expr =
        match expr.pexp_desc with
        | Pexp_fun
            (label, _, ({ ppat_desc = Ppat_var arg; _ } as pat), expr) ->
            collect_props ((label, pat, arg) :: acc) expr
        | Pexp_fun (_, _, _, _) -> failwith "not an arg"
        | _ -> acc
      in
      collect_props [] expr
    in
    let unpack_expr =
      let args =
        List.rev_map
          (fun (label, _pat, name) ->
            label, [%expr props ## [%e ident name]])
          args
      in
      pexp_apply ~loc (ident label) args
    in
    let pack_expr =
      let fields =
        List.rev_map (fun (_, _, name) -> longident name, ident name) args
      in
      let props = pexp_record ~loc fields None in
      ListLabels.fold_left args
        ~init:
          [%expr
            React.unsafe_create_element [%e ident label]
              [%bs.obj [%e props]]]
        ~f:(fun body (label, pat, _name) ->
          pexp_fun ~loc label None pat body)
    in
    [%stri
      let [%p pat] =
        let [%p pat] = [%e expr] in
        let [%p pat] = fun props -> [%e unpack_expr] in
        [%e pack_expr]]

  let ext =
    let pattern =
      let open Ast_pattern in
      let extractor_in_let =
        pstr_value drop (value_binding ~pat:__ ~expr:__ ^:: nil)
      in
      pstr @@ extractor_in_let ^:: nil
    in
    Context_free.Rule.extension
      (Extension.V3.declare "component" Extension.Context.structure_item
         pattern expand)
end

module Ext_export_component = struct
  let deriving_yojson ~loc =
    {
      attr_loc = loc;
      attr_name = { txt = "deriving"; loc };
      attr_payload = PStr [ [%stri yojson_of] ];
    }

  let expand_export ~ctxt items =
    let open Ast_builder.Default in
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let props = ref None in
    let items =
      List.map
        (fun stri ->
          match stri.pstr_desc with
          | Pstr_type (f, ts) ->
              let ts =
                List.map
                  (function
                    | {
                        ptype_name = { txt = "props"; _ };
                        ptype_kind = Ptype_record fs;
                        _;
                      } as t ->
                        assert (Option.is_none !props);
                        props := Some fs;
                        t
                    | t -> t)
                  ts
              in
              { stri with pstr_desc = Pstr_type (f, ts) }
          | _ -> stri)
        items
    in
    let props =
      match !props with
      | Some props -> props
      | None -> failwith "no props type found"
    in
    let props_fields =
      List.fold_left
        (fun xs { pld_name; pld_type; pld_loc; _ } ->
          let lab = longident pld_name in
          let prop = pexp_field ~loc:pld_loc [%expr props] lab in
          let name =
            pexp_constant ~loc (Pconst_string (pld_name.txt, loc, None))
          in
          let value =
            match pld_type.ptyp_desc with
            | Ptyp_constr ({ txt = ident; loc }, [])
              when Longident.name ident = "element" ->
                [%expr `Element [%e prop]]
            | _ ->
                [%expr
                  ([%yojson_of: [%t pld_type]] [%e prop]
                    :> [ json | `Element of React.element ])]
          in
          [%expr ([%e name], [%e value]) :: [%e xs]])
        [%expr []] props
    in
    [ [%stri open Ppx_yojson_conv_lib.Yojson_conv.Primitives] ]
    @ items
    @ [
        [%stri
          let make props =
            React_server.React_element.client_thunk "App"
              [%e props_fields]];
      ]

  let expand ~ctxt name expr =
    let open Ast_builder.Default in
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match expr.pmod_desc with
    | Pmod_structure items ->
        let items =
          match !mode with
          | Melange ->
              items
              @ [%str
                  let make props = React.unsafe_create_element make props
                  let () = React.Exported_components.register "App" make]
          | Export -> expand_export ~ctxt items
        in
        pstr_module ~loc
          (module_binding ~loc ~name:{ loc; txt = name }
             ~expr:{ expr with pmod_desc = Pmod_structure items })
    | _ -> failwith "not a structure"

  let ext =
    let pattern =
      let open Ast_pattern in
      pstr @@ pstr_module (module_binding ~name:__ ~expr:__) ^:: nil
    in
    Context_free.Rule.extension
      (Extension.V3.declare "export_component"
         Extension.Context.structure_item pattern expand)
end

let rec cleanup_all xs =
  List.filter_map
    (fun (x : structure_item) ->
      match x.pstr_desc with
      | Pstr_eval _ -> None
      | Pstr_value _ -> None
      | Pstr_primitive _ -> None
      | Pstr_type _ -> Some x
      | Pstr_typext _ -> Some x
      | Pstr_exception _ -> Some x
      | Pstr_module
          ({
             pmb_expr = { pmod_desc = Pmod_structure str; _ } as pmb_expr;
             _;
           } as pmb) ->
          let str = cleanup_all str in
          Some
            {
              x with
              pstr_desc =
                Pstr_module
                  {
                    pmb with
                    pmb_expr =
                      { pmb_expr with pmod_desc = Pmod_structure str };
                  };
            }
      | Pstr_module _ -> None
      | Pstr_recmodule _ -> None
      | Pstr_modtype _ -> Some x
      | Pstr_open _ -> Some x
      | Pstr_class _ -> None
      | Pstr_class_type _ -> None
      | Pstr_include _ -> None
      | Pstr_attribute _ -> None
      | Pstr_extension ((name, PStr str), attrs)
        when name.txt = "export_component" ->
          let str = cleanup_all str in
          Some
            {
              x with
              pstr_desc = Pstr_extension ((name, PStr str), attrs);
            }
      | Pstr_extension _ -> None)
    xs

let cleanup_all xs =
  let loc = Location.none in
  match !mode with
  | Melange -> [%stri open React_browser] :: xs
  | Export -> [%stri open React_server] :: cleanup_all xs

let () =
  Driver.add_arg "-export"
    (Unit (fun () -> mode := Export))
    ~doc:"only keep RSC export";
  Driver.register_transformation
    ~rules:[ Ext_component.ext; Ext_export_component.ext ]
    ~preprocess_impl:cleanup_all "react_jsx"
