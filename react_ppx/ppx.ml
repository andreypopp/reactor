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
  open Ast_builder.Default

  type t = { name : label loc; props : prop list; body : expression }

  and prop = {
    arg_label : arg_label;
    expr : expression option;
    pat : pattern;
    typ : core_type option;
    label : label loc;
    label_synthetic : bool;
  }

  let parse ~loc pat expr =
    let name =
      match pat.ppat_desc with
      | Ppat_var label -> label
      | _ ->
          raise_errorf ~loc
            "%%component: should only be applied to functions"
    in
    let seen_unit, props, body =
      let key_prop =
        let label = { txt = "key"; loc } in
        {
          arg_label = Optional "key";
          expr = None;
          pat = ppat_var ~loc label;
          label;
          label_synthetic = false;
          typ = Some [%type: string option];
        }
      in
      let rec collect_props ~seen_unit acc expr =
        match expr.pexp_desc with
        | Pexp_fun
            ( ((Labelled arg | Optional arg) as arg_label),
              default,
              pat,
              expr ) ->
            let typ =
              match pat.ppat_desc with
              | Ppat_constraint (_, typ) -> Some typ
              | _ -> None
            in
            collect_props ~seen_unit
              ({
                 arg_label;
                 expr = default;
                 pat;
                 label = { txt = arg; loc = pat.ppat_loc };
                 label_synthetic = false;
                 typ;
               }
              :: acc)
              expr
        | Pexp_fun (Nolabel, _default, pat, expr) -> (
            match pat.ppat_desc with
            | Ppat_construct ({ txt = Lident "()"; _ }, None)
              when not seen_unit ->
                collect_props ~seen_unit:true acc expr
            | _ ->
                raise_errorf ~loc:pat.ppat_loc
                  "%%component: only labelled arguments are allowed")
        | _ -> seen_unit, acc, expr
      in
      collect_props ~seen_unit:false [ key_prop ] expr
    in
    let () =
      match seen_unit with
      | true -> ()
      | false -> raise_errorf ~loc "missing () argument"
    in
    { name; props; body }
end

module Ext_component = struct
  open Ast_builder.Default

  let unit_arg ~loc () =
    Nolabel, pexp_construct ~loc { txt = Lident "()"; loc } None

  let expand_js ~ctxt:_ ~loc component pat expr =
    let make_elem =
      let args =
        ListLabels.fold_left component.Let_component.props
          ~f:(fun args arg ->
            match arg.Let_component.label.txt with
            | "key" -> args
            | _ ->
                (arg.arg_label, [%expr props ## [%e ident arg.label]])
                :: args)
          ~init:[ unit_arg ~loc () ]
      in
      pexp_apply ~loc (ident component.name) args
    in
    let make_props =
      let fields =
        ListLabels.fold_left component.props ~init:[] ~f:(fun fs prop ->
            let k = longident prop.Let_component.label in
            match prop.Let_component.label.txt with
            | "children" -> (k, [%expr React.null]) :: fs
            | _ -> (k, ident prop.label) :: fs)
      in
      let init =
        let props = pexp_record ~loc fields None in
        [%expr fun () -> [%mel.obj [%e props]]]
      in
      ListLabels.fold_left component.props ~init ~f:(fun body arg ->
          match arg.Let_component.label.txt with
          | "children" -> body
          | _ ->
              pexp_fun ~loc arg.Let_component.arg_label None
                (ppat_var ~loc:pat.ppat_loc arg.label)
                body)
    in
    let pat_props =
      ppat_var ~loc { txt = sprintf "%s__props" component.name.txt; loc }
    in
    [%stri
      include struct
        let [%p pat] = [%e expr]
        let [%p pat] = fun props -> [%e make_elem]
        let [%p pat_props] = [%e make_props]
      end]

  let expand_native ctor ~ctxt:_ ~loc component pat _expr =
    let make_elem =
      ListLabels.fold_left component.props
        ~init:
          [%expr
            fun () ->
              let _ = key in
              [%e ctor] (fun () -> [%e component.Let_component.body])]
        ~f:(fun
            body
            {
              Let_component.arg_label;
              expr;
              pat;
              typ = _;
              label = _;
              label_synthetic = _;
            }
          -> pexp_fun ~loc arg_label expr pat body)
    in
    [%stri let [%p pat] = [%e make_elem]]

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

  let component_id ~ctxt name =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let fname = loc.loc_start.pos_fname in
    estring ~loc (sprintf "%s#%s" fname name)

  let props_to_model ~ctxt (props : Let_component.prop list) =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    List.fold_left
      (fun xs { Let_component.label; typ; _ } ->
        match typ with
        | None -> pexp_errorf ~loc:label.loc "missing type annotation"
        | Some typ ->
            let prop = pexp_ident ~loc (longident label) in
            let name = estring ~loc label.txt in
            let value =
              match typ.ptyp_desc with
              | Ptyp_constr ({ txt = ident; loc }, [])
                when Longident.name ident = "element"
                     || Longident.name ident = "React.element" ->
                  [%expr React_server.React.Element [%e prop]]
              | Ptyp_constr ({ txt = ident; loc }, [ typ ])
                when Longident.name ident = "promise"
                     || Longident.name ident = "Promise.t" ->
                  [%expr
                    React_server.React.Promise
                      ([%e prop], [%yojson_of: [%t typ]])]
              | _ ->
                  [%expr
                    let json = [%to_json: [%t typ]] [%e prop] in
                    let json = Yojson.Basic.to_string json in
                    React_server.React.Json json]
            in
            [%expr ([%e name], [%e value]) :: [%e xs]])
      [%expr []] props

  let call_from_server ~ctxt (component : Let_component.t) =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let args =
      List.rev_map
        (fun { Let_component.arg_label; typ; label; _ } ->
          match typ with
          | None ->
              let value =
                pexp_errorf ~loc:label.loc "missing type annotation"
              in
              arg_label, value
          | Some typ ->
              let name = estring ~loc label.txt in
              let value =
                match typ.ptyp_desc with
                | Ptyp_constr ({ txt = ident; loc }, [])
                  when Longident.name ident = "element"
                       || Longident.name ident = "React.element" ->
                    [%expr
                      (Obj.magic Js.Dict.unsafeGet props [%e name]
                        : React.element)]
                | Ptyp_constr ({ txt = ident; loc }, [ typ ])
                  when Longident.name ident = "promise"
                       || Longident.name ident = "Promise.t" ->
                    [%expr
                      let promise = Js.Dict.unsafeGet props [%e name] in
                      let promise' =
                        (Obj.magic promise : [%t typ] Promise.t Js.Dict.t)
                      in
                      match Js.Dict.get promise' "__promise" with
                      | Some promise -> promise
                      | None ->
                          let promise =
                            Promise.(
                              let* json =
                                (Obj.magic (Js.Promise.resolve promise)
                                  : string Promise.t)
                              in
                              let json = Js.Json.parseExn json in
                              let data = [%of_json: [%t typ]] json in
                              return data)
                          in
                          Js.Dict.set promise' "__promise" promise;
                          promise]
                | _ ->
                    [%expr
                      let json = Js.Dict.unsafeGet props [%e name] in
                      let json = Js.Json.parseExn json in
                      [%of_json: [%t typ]] json]
              in
              arg_label, value)
        component.props
    in
    let props =
      let props_name =
        {
          component.name with
          txt = sprintf "%s__props" component.name.txt;
        }
      in
      pexp_apply ~loc
        (pexp_ident ~loc (longident props_name))
        (( Nolabel,
           pexp_construct ~loc:Location.none
             { txt = Lident "()"; loc = Location.none }
             None )
        :: args)
    in
    (* TODO: need to pass children properly *)
    [%expr
      React.unsafe_create_element
        [%e pexp_ident ~loc (longident component.name)]
        [%e props] [||]]

  let expand_native ~ctxt (component : Let_component.t) pat _expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let expr =
      let body =
        [%expr
          fun () ->
            React_server.React.client_thunk
              [%e component_id ~ctxt component.name.txt]
              [%e props_to_model ~ctxt component.props]
              (React_server.React.thunk (fun () -> [%e component.body]))]
      in
      ListLabels.fold_left component.props ~init:body
        ~f:(fun
            body
            {
              Let_component.arg_label;
              expr;
              pat;
              typ = _;
              label;
              label_synthetic;
            }
          ->
          let pat =
            if label_synthetic then ppat_alias ~loc pat label else pat
          in
          pexp_fun ~loc arg_label expr pat body)
    in
    [%stri let [%p pat] = [%e expr]]

  let expand_js ~ctxt (component : Let_component.t) pat expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let js_component =
      Ext_component.expand_js ~ctxt ~loc component pat expr
    in
    [%stri
      include struct
        [%%i js_component]

        let () =
          React_browser.Component_map.register
            [%e component_id ~ctxt component.name.txt] (fun props ->
              [%e call_from_server ~ctxt component])
      end]

  let expand ~ctxt pat expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    try
      let component = Let_component.parse ~loc pat expr in
      match !mode with
      | Target_js -> expand_js ~ctxt component pat expr
      | Target_native -> expand_native ~ctxt component pat expr
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

  let expand ~ctxt payload =
    let mode = !mode in
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    try
      match payload with
      | `Let_form (orig_expr, recflag, bindings, body) -> (
          match mode with
          | Target_js -> orig_expr
          | Target_native ->
              let bindings = build_bindings ~ctxt bindings in
              pexp_let ~loc:orig_expr.pexp_loc recflag bindings body)
      | `Fun_form (orig_expr, recflag, default, pat) -> (
          match mode with
          | Target_js -> orig_expr
          | Target_native ->
              pexp_fun ~loc recflag default pat
                [%expr raise React_server.React.Browser_only])
    with Error err -> err

  let ext =
    let pattern =
      let open Ast_pattern in
      let let_form =
        single_expr_payload
          (as__
             (pexp_let __
                (many
                   (value_binding ~pat:__ ~expr:__
                   |> map2 ~f:(fun a b -> a, b)))
                __))
        |> map2 ~f:(fun a b -> a, b)
        |> map2 ~f:(fun (a, b) c -> a, b, c)
        |> map2 ~f:(fun (a, b, c) d -> `Let_form (a, b, c, d))
      in
      let fun_form =
        single_expr_payload (as__ (pexp_fun __ __ __ drop))
        |> map2 ~f:(fun a b -> a, b)
        |> map2 ~f:(fun (a, b) c -> a, b, c)
        |> map2 ~f:(fun (a, b, c) d -> `Fun_form (a, b, c, d))
      in
      let_form ||| fun_form
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

module Jsx = struct
  open Ast_builder.Default

  let collect_props visit args =
    let rec collect_props (dangerouslySetInnerHTML, props) = function
      | [] -> None, dangerouslySetInnerHTML, props
      | [ (Nolabel, arg) ] ->
          Some (visit arg), dangerouslySetInnerHTML, props
      | (Nolabel, arg) :: _ ->
          raise_errorf ~loc:arg.pexp_loc
            "an argument without a label could only be the last one"
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

  let rec unwrap_children ~f children = function
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ }
      ->
        List.rev children
    | {
        pexp_desc =
          Pexp_construct
            ( { txt = Lident "::"; _ },
              Some { pexp_desc = Pexp_tuple [ child; next ]; _ } );
        _;
      } ->
        unwrap_children ~f (f child :: children) next
    | e ->
        raise_errorf ~loc:e.pexp_loc "JSX: children prop should be a list"

  let user_component_props id =
    match id.txt with
    | Lident name -> { id with txt = Lident (sprintf "%s__props" name) }
    | Ldot (prefix, name) ->
        { id with txt = Ldot (prefix, sprintf "%s__props" name) }
    | Lapply _ -> assert false

  let has_jsx_attr attrs =
    List.exists
      (function
        | { attr_name = { txt = "JSX"; _ }; _ } -> true | _ -> false)
      attrs

  let jsx_rewrite ~extract_dangerouslySetInnerHTML ~rewrite_child
      ~rewrite_element =
    object (self)
      inherit Ast_traverse.map as super

      method! expression : expression -> expression =
        fun expr ->
          match expr.pexp_desc with
          | Pexp_apply (tag, args) when has_jsx_attr expr.pexp_attributes
            ->
              let loc = expr.pexp_loc in
              let tagname =
                match tag.pexp_desc with
                | Pexp_ident id -> id
                | _ ->
                    raise_errorf ~loc:tag.pexp_loc
                      "JSX tag should be an identifier"
              in
              let children = ref (Location.none, []) in
              let dangerouslySetInnerHTML = ref None in
              let args =
                ListLabels.filter_map args ~f:(function
                  | Labelled "children", e ->
                      let children' =
                        unwrap_children [] e ~f:(fun e ->
                            let e = rewrite_child e in
                            self#expression e)
                      in
                      children := e.pexp_loc, children';
                      None
                  | ( ( Labelled "dangerouslySetInnerHTML"
                      | Optional "dangerouslySetInnerHTML" ),
                      _e ) as arg
                    when extract_dangerouslySetInnerHTML ->
                      dangerouslySetInnerHTML := Some arg;
                      None
                  | arg_label, e -> Some (arg_label, self#expression e))
              in
              let children =
                match !children with
                | _loc, [] -> None
                | loc, children -> Some (pexp_array ~loc children)
              in
              let tag =
                match tag.pexp_desc with
                | Pexp_ident { txt = Lident name; loc = name_loc }
                  when Html.is_html name ->
                    let name = estring ~loc:name_loc name in
                    `Html_component name
                | _ -> `User_component tag
              in
              rewrite_element ~loc ~tagname
                ~dangerouslySetInnerHTML:!dangerouslySetInnerHTML
                ~children ~tag ~props:args ()
          | _ -> super#expression expr
    end

  let jsx_rewrite_js =
    jsx_rewrite ~extract_dangerouslySetInnerHTML:false
      ~rewrite_child:(fun e ->
        match e.pexp_desc with
        | Pexp_constant (Pconst_string _) ->
            let loc = e.pexp_loc in
            [%expr React.text [%e e]]
        | _ -> e)
      ~rewrite_element:(fun
          ~loc
          ~tagname
          ~dangerouslySetInnerHTML:_
          ~children
          ~tag
          ~props
          ()
        ->
        match tag with
        | `Html_component name ->
            let children = Option.value children ~default:[%expr [||]] in
            let props =
              pexp_apply ~loc [%expr React_browser_html_props.props] props
            in
            [%expr
              React.unsafe_create_html_element [%e name] [%e props]
                [%e children]]
        | `User_component component -> (
            let props =
              pexp_apply ~loc
                (pexp_ident ~loc (user_component_props tagname))
                props
            in
            match children with
            | None ->
                [%expr
                  React.unsafe_create_element' [%e component] [%e props]]
            | Some children ->
                [%expr
                  React.unsafe_create_element [%e component] [%e props]
                    [%e children]]))

  let jsx_rewrite_native =
    jsx_rewrite ~extract_dangerouslySetInnerHTML:true
      ~rewrite_child:(fun e ->
        match e.pexp_desc with
        | Pexp_constant (Pconst_string _) ->
            let loc = e.pexp_loc in
            [%expr React_server.React.text [%e e]]
        | _ -> e)
      ~rewrite_element:(fun
          ~loc
          ~tagname:_
          ~dangerouslySetInnerHTML
          ~children
          ~tag
          ~props
          ()
        ->
        match tag with
        | `Html_component name -> (
            let make_props =
              match props with
              | [] -> [%expr []]
              | props ->
                  List.fold_left
                    (fun xs (label, x) ->
                      match label with
                      | Nolabel -> xs
                      | Optional _ -> assert false
                      | Labelled name -> (
                          match Html.browser_only_prop name with
                          | true -> xs
                          | false ->
                              let make =
                                pexp_ident ~loc:x.pexp_loc
                                  {
                                    txt =
                                      Longident.parse
                                        (sprintf
                                           "React_server.React.Html_props.%s"
                                           name);
                                    loc = x.pexp_loc;
                                  }
                              in
                              [%expr [%e make] [%e x] :: [%e xs]]))
                    [%expr []] props
            in
            match children, dangerouslySetInnerHTML with
            | None, Some dangerouslySetInnerHTML ->
                [%expr
                  React_server.React.unsafe_create_html_element [%e name]
                    [%e make_props]
                    (Some
                       (React_server.React.Html_children_raw
                          [%e snd dangerouslySetInnerHTML]))]
            | Some _, Some _ ->
                pexp_errorf ~loc
                  "both children and dangerouslySetInnerHTML cannot be \
                   used at once"
            | Some children, None ->
                [%expr
                  React_server.React.unsafe_create_html_element [%e name]
                    [%e make_props]
                    (Some (React_server.React.Html_children [%e children]))]
            | None, None ->
                [%expr
                  React_server.React.unsafe_create_html_element [%e name]
                    [%e make_props] None])
        | `User_component component ->
            let props =
              match children with
              | None -> props
              | Some children -> (Labelled "children", children) :: props
            in
            pexp_apply ~loc component props)

  let run xs =
    let loc = Location.none in
    try
      match !mode with
      | Target_js -> jsx_rewrite_js#structure xs
      | Target_native -> jsx_rewrite_native#structure xs
    with Error err -> [%str [%e err]]
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
    ~preprocess_impl:Jsx.run "react_jsx"
