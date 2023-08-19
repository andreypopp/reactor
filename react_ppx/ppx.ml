open Printf
open Ppxlib

type target = Target_native | Target_js

let mode = ref Target_js

let pexp_errorf ~loc fmt =
  let open Ast_builder.Default in
  ksprintf
    (fun msg ->
      pexp_extension ~loc (Location.error_extensionf ~loc "%s" msg))
    fmt

exception Error of expression

let raise_errorf ~loc fmt =
  let open Ast_builder.Default in
  ksprintf
    (fun msg ->
      let expr =
        pexp_extension ~loc (Location.error_extensionf ~loc "%s" msg)
      in
      raise (Error expr))
    fmt

let longident label =
  { txt = Longident.Lident label.txt; loc = label.loc }

let ident label =
  let open Ast_builder.Default in
  pexp_ident ~loc:label.loc (longident label)

module Let_component = struct
  type t = {
    name : label loc;
    props : (arg_label * expression option * pattern * label loc) list;
    body : expression;
  }

  let parse ~loc pat expr =
    let name =
      match pat.ppat_desc with
      | Ppat_var label -> label
      | _ ->
          raise_errorf ~loc
            "let%%component should only be applied to functions"
    in
    let props, body =
      let rec collect_props n acc expr =
        match expr.pexp_desc with
        | Pexp_fun
            ( label,
              default,
              ({ ppat_desc = Ppat_var arg; _ } as pat),
              expr ) ->
            collect_props (n + 1) ((label, default, pat, arg) :: acc) expr
        | Pexp_fun
            (((Labelled arg | Optional arg) as label), default, pat, expr)
          ->
            collect_props (n + 1)
              ((label, default, pat, { txt = arg; loc = pat.ppat_loc })
              :: acc)
              expr
        | Pexp_fun ((Nolabel as label), default, pat, expr) ->
            let arg = { txt = sprintf "prop_%i" n; loc = pat.ppat_loc } in
            collect_props (n + 1) ((label, default, pat, arg) :: acc) expr
        | Pexp_function _ ->
            raise_errorf ~loc:expr.pexp_loc
              "component arguments can only be simple patterns"
        | _ -> acc, expr
      in
      collect_props 0 [] expr
    in
    { name; props; body }
end

module Ext_component = struct
  open Ast_builder.Default

  let expand_js ~ctxt:_ ~loc component pat expr =
    let unpack_expr =
      let args =
        List.rev_map
          (fun (label, _default, _pat, name) ->
            label, [%expr props ## [%e ident name]])
          component.Let_component.props
      in
      pexp_apply ~loc (ident component.name) args
    in
    let pack_expr =
      let fields =
        List.rev_map
          (fun (_, _, _, name) -> longident name, ident name)
          component.props
      in
      let props = pexp_record ~loc fields None in
      ListLabels.fold_left component.props
        ~init:
          [%expr
            React.unsafe_create_element [%e ident component.name]
              [%mel.obj [%e props]]]
        ~f:(fun body (label, _default, pat, name) ->
          pexp_fun ~loc label None (ppat_var ~loc:pat.ppat_loc name) body)
    in
    [%stri
      let [%p pat] =
        let [%p pat] = [%e expr] in
        let [%p pat] = fun props -> [%e unpack_expr] in
        [%e pack_expr]]

  let expand_native ctor ~ctxt:_ ~loc component pat _expr =
    let pack_expr =
      ListLabels.fold_left component.props
        ~init:
          [%expr [%e ctor] (fun () -> [%e component.Let_component.body])]
        ~f:(fun body (label, default, pat, _name) ->
          pexp_fun ~loc label default pat body)
    in
    [%stri let [%p pat] = [%e pack_expr]]

  let expand ~ctxt pat expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    try
      let component = Let_component.parse ~loc pat expr in
      match !mode with
      | Target_js -> expand_js ~ctxt ~loc component pat expr
      | Target_native ->
          expand_native [%expr React_server.React.thunk] ~ctxt ~loc
            component pat expr
    with Error err -> [%stri let [%p pat] = [%e err]]

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

module Ext_async_component = struct
  let expand ~ctxt pat expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    try
      match !mode with
      | Target_js ->
          let err =
            pexp_errorf ~loc
              "async components are not supported in browser"
          in
          [%stri let [%p pat] = [%e err]]
      | Target_native ->
          let component = Let_component.parse ~loc pat expr in
          Ext_component.expand_native
            [%expr React_server.React.async_thunk] ~ctxt ~loc component
            pat expr
    with Error err -> [%stri let [%p pat] = [%e err]]

  let ext =
    let pattern =
      let open Ast_pattern in
      let extractor_in_let =
        pstr_value drop (value_binding ~pat:__ ~expr:__ ^:: nil)
      in
      pstr @@ extractor_in_let ^:: nil
    in
    Context_free.Rule.extension
      (Extension.V3.declare "async_component"
         Extension.Context.structure_item pattern expand)
end

module Ext_export_component = struct
  open Ast_builder.Default

  let deriving_yojson ~loc =
    {
      attr_loc = loc;
      attr_name = { txt = "deriving"; loc };
      attr_payload = PStr [ [%stri yojson_of] ];
    }

  let expand_native ~ctxt items =
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
                    :> [ React_server.json | `Element of React.element ])]
          in
          [%expr ([%e name], [%e value]) :: [%e xs]])
        [%expr []] props
    in
    [ [%stri open Ppx_yojson_conv_lib.Yojson_conv.Primitives] ]
    @ items
    @ [
        [%stri
          let make props =
            React_server.React.client_thunk "App" [%e props_fields]
              (React_server.React.thunk (fun () -> make props))];
      ]

  let expand_js ~ctxt items =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    items
    @ [%str
        let make props = React.unsafe_create_element make props
        let () = React_browser.Component_map.register "App" make]

  let expand ~ctxt name expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match expr.pmod_desc with
    | Pmod_structure items ->
        let items =
          match !mode with
          | Target_js -> expand_js ~ctxt items
          | Target_native -> expand_native ~ctxt items
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

module Browser_only_expression = struct
  open Ast_builder.Default

  let expand_binding_native ~ctxt (pat, expr) =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match pat with
    | [%pat? ()] -> pat, [%expr ()]
    | _ -> (
        match expr.pexp_desc with
        | Pexp_fun _ | Pexp_function _ ->
            pat, [%expr fun _ -> raise React_server.React.Browser_only]
        | Pexp_lazy _ ->
            pat, [%expr lazy (raise React_server.React.Browser_only)]
        | _ ->
            raise_errorf ~loc:pat.ppat_loc
              "Invalid %%browser_only usage, only the following is \
               allowed:\n\
              \  let%%browser_only () = ...\n\
              \  let%%browser_only func arg1 ... = ...")

  let build_bindings ~ctxt bindings =
    List.map (expand_binding_native ~ctxt) bindings
    |> List.map (fun (pat, expr) ->
           value_binding ~loc:pat.ppat_loc ~pat ~expr)

  let expand ~ctxt orig_expr recflag bindings body =
    try
      match !mode with
      | Target_js -> orig_expr
      | Target_native ->
          let bindings = build_bindings ~ctxt bindings in
          pexp_let ~loc:orig_expr.pexp_loc recflag bindings body
    with Error err -> err

  let ext =
    let pattern =
      let open Ast_pattern in
      single_expr_payload
        (as__
           (pexp_let __
              (many
                 (value_binding ~pat:__ ~expr:__
                 |> map2 ~f:(fun a b -> a, b)))
              __))
    in
    Context_free.Rule.extension
      (Extension.V3.declare "browser_only" Extension.Context.expression
         pattern expand)
end

module Browser_only_structure_item = struct
  open Ast_builder.Default

  let expand ~ctxt orig_stri recflag bindings =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    try
      match !mode with
      | Target_js -> orig_stri
      | Target_native ->
          let bindings =
            Browser_only_expression.build_bindings ~ctxt bindings
          in
          pstr_value ~loc:orig_stri.pstr_loc recflag bindings
    with Error err -> [%stri let [%e ()] = [%e err]]

  let ext =
    let pattern =
      let open Ast_pattern in
      pstr
      @@ as__
           (pstr_value __
              (many
                 (value_binding ~pat:__ ~expr:__
                 |> map2 ~f:(fun a b -> a, b))))
      ^:: nil
    in
    Context_free.Rule.extension
      (Extension.V3.declare "browser_only"
         Extension.Context.structure_item pattern expand)
end

let preprocess_impl xs =
  let loc = Location.none in
  match !mode with
  | Target_js -> [%stri open React_browser] :: xs
  | Target_native -> [%stri open React_server.React_browser] :: xs

module Jsx = struct
  open Ast_builder.Default

  let collect_props visit args =
    let rec collect_props (dangerouslySetInnerHTML, props) = function
      | [] -> None, dangerouslySetInnerHTML, props
      | [ (Nolabel, arg) ] ->
          Some (visit arg), dangerouslySetInnerHTML, props
      | (Nolabel, arg) :: _ ->
          raise_errorf ~loc:arg.pexp_loc
            "an argument without a label could on be the last one"
      | ( (( Optional "dangerouslySetInnerHTML"
           | Labelled "dangerouslySetInnerHTML" ) as label),
          prop )
        :: xs ->
          collect_props (Some (label, prop), props) xs
      | (proplab, prop) :: xs ->
          collect_props
            (dangerouslySetInnerHTML, (proplab, visit prop) :: props)
            xs
    in
    collect_props (None, []) args

  let jsx_rewrite_js =
    object
      inherit Ast_traverse.map as super

      method! expression : expression -> expression =
        fun expr ->
          match expr.pexp_desc with
          | Pexp_apply
              ({ pexp_desc = Pexp_field ([%expr jsx], id); _ }, args) -> (
              let id =
                match id.txt with
                | Lident lab ->
                    pexp_constant ~loc:id.loc
                      (Pconst_string (lab, id.loc, None))
                | _ ->
                    pexp_errorf ~loc:id.loc "should be a DOM element name"
              in
              let children, dangerouslySetInnerHTML, props =
                collect_props super#expression args
              in
              let props =
                match dangerouslySetInnerHTML with
                | None -> props
                | Some dangerouslySetInnerHTML ->
                    dangerouslySetInnerHTML :: props
              in
              let loc = expr.pexp_loc in
              let make_props =
                match props with
                | [] -> [%expr React_browser.React.html_props_null]
                | props ->
                    pexp_apply ~loc [%expr React_browser.React.html_props]
                      ((Nolabel, [%expr ()]) :: props)
              in
              match children, dangerouslySetInnerHTML with
              | Some _, Some _ ->
                  pexp_errorf ~loc
                    "both children and dangerouslySetInnerHTML cannot be \
                     used at once"
              | Some children, None ->
                  [%expr
                    React_browser.React.unsafe_create_html_element [%e id]
                      [%e make_props] [%e children]]
              | None, _ ->
                  [%expr
                    React_browser.React.unsafe_create_html_element [%e id]
                      [%e make_props] [||]])
          | _ -> super#expression expr
    end

  let js_only_prop = function "onClick" -> true | _ -> false

  let jsx_rewrite_native =
    object
      inherit Ast_traverse.map as super

      method! expression : expression -> expression =
        fun expr ->
          match expr.pexp_desc with
          | Pexp_apply
              ({ pexp_desc = Pexp_field ([%expr jsx], id); _ }, args) -> (
              let id =
                match id.txt with
                | Lident lab ->
                    pexp_constant ~loc:id.loc
                      (Pconst_string (lab, id.loc, None))
                | _ ->
                    pexp_errorf ~loc:id.loc "should be a DOM element name"
              in
              let children, dangerouslySetInnerHTML, props =
                collect_props super#expression args
              in
              let dangerouslySetInnerHTML =
                Option.map snd dangerouslySetInnerHTML
              in
              let loc = expr.pexp_loc in
              let make_props =
                match props with
                | [] -> [%expr []]
                | props ->
                    List.fold_left
                      (fun xs (label, x) ->
                        let name =
                          match label with
                          | Nolabel -> assert false
                          | Optional _ -> assert false
                          | Labelled name -> name
                        in
                        if js_only_prop name then xs
                        else
                          let make =
                            pexp_ident ~loc:x.pexp_loc
                              {
                                txt =
                                  Longident.parse
                                    (sprintf
                                       "React_server.React.Html_prop.%s"
                                       name);
                                loc = x.pexp_loc;
                              }
                          in
                          [%expr [%e make] [%e x] :: [%e xs]])
                      [%expr []] props
              in

              match children, dangerouslySetInnerHTML with
              | Some _, Some _ ->
                  pexp_errorf ~loc
                    "both children and dangerouslySetInnerHTML cannot be \
                     used at once"
              | None, Some dangerouslySetInnerHTML ->
                  [%expr
                    React_server.React.unsafe_create_html_element [%e id]
                      [%e make_props]
                      (Some
                         (React_server.React.Html_children_raw
                            [%e dangerouslySetInnerHTML]))]
              | Some children, None ->
                  [%expr
                    React_server.React.unsafe_create_html_element [%e id]
                      [%e make_props]
                      (Some
                         (React_server.React.Html_children [%e children]))]
              | None, None ->
                  [%expr
                    React_server.React.unsafe_create_html_element [%e id]
                      [%e make_props] None])
          | _ -> super#expression expr
    end

  let run xs =
    match !mode with
    | Target_js -> jsx_rewrite_js#structure xs
    | Target_native -> jsx_rewrite_native#structure xs
end

let () =
  Driver.add_arg "-native"
    (Unit (fun () -> mode := Target_native))
    ~doc:"preprocess for native build";
  Driver.register_transformation
    ~rules:
      [
        Ext_component.ext;
        Ext_async_component.ext;
        Ext_export_component.ext;
        Browser_only_expression.ext;
        Browser_only_structure_item.ext;
      ]
    ~impl:Jsx.run ~preprocess_impl "react_jsx"
