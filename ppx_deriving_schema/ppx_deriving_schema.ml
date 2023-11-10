open Printf
open Ppxlib
open Ast_builder.Default
open ContainersLabels

exception Not_supported of location * string

let not_supported ~loc what =
  raise (Not_supported (loc, sprintf "%s are not supported" what))

module Repr = struct
  type type_decl = {
    name : label loc;
    params : label loc list;
    shape : type_decl_shape;
    loc : location;
  }

  and type_decl_shape =
    | Ts_record of (label loc * type_expr) list
    | Ts_variant of variant_case list
    | Ts_expr of type_expr

  and type_expr' =
    | Te_opaque of Longident.t loc * type_expr list
    | Te_var of label loc
    | Te_tuple of type_expr list
    | Te_polyvariant of polyvariant_case list

  and type_expr = core_type * type_expr'

  and variant_case =
    | Vc_tuple of label loc * type_expr list
    | Vc_record of label loc * (label loc * type_expr) list

  and polyvariant_case =
    | Pvc_construct of label loc * type_expr list
    | Pvc_inherit of Longident.t loc * type_expr list

  let rec of_core_type (typ : Parsetree.core_type) : type_expr =
    let loc = typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_tuple ts -> typ, Te_tuple (List.map ts ~f:of_core_type)
    | Ptyp_constr (id, ts) ->
        typ, Te_opaque (id, List.map ts ~f:(fun t -> of_core_type t))
    | Ptyp_variant (fields, Closed, None) ->
        let cs =
          List.map fields ~f:(fun field ->
              match field.prf_desc with
              | Rtag (id, _, ts) ->
                  Pvc_construct (id, List.map ts ~f:of_core_type)
              | Rinherit { ptyp_desc = Ptyp_constr (id, ts); _ } ->
                  Pvc_inherit (id, List.map ts ~f:of_core_type)
              | Rinherit _ ->
                  not_supported ~loc:field.prf_loc
                    "this polyvariant inherit")
        in
        typ, Te_polyvariant cs
    | Ptyp_variant _ -> not_supported ~loc "non closed polyvariants"
    | Ptyp_arrow _ -> not_supported ~loc "function types"
    | Ptyp_any -> not_supported ~loc "type placeholders"
    | Ptyp_var label -> typ, Te_var { txt = label; loc = typ.ptyp_loc }
    | Ptyp_object _ -> not_supported ~loc "object types"
    | Ptyp_class _ -> not_supported ~loc "class types"
    | Ptyp_poly _ -> not_supported ~loc "polymorphic type expressions"
    | Ptyp_package _ -> not_supported ~loc "packaged module types"
    | Ptyp_extension _ -> not_supported ~loc "extension nodes"
    | Ptyp_alias _ -> not_supported ~loc "type aliases"

  let of_type_declaration (td : Parsetree.type_declaration) : type_decl =
    let loc = td.ptype_loc in
    let shape =
      match td.ptype_kind, td.ptype_manifest with
      | Ptype_abstract, None -> not_supported ~loc "abstract types"
      | Ptype_abstract, Some t -> Ts_expr (of_core_type t)
      | Ptype_variant ctors, _ ->
          let cs =
            List.map ctors ~f:(fun ctor ->
                match ctor.pcd_args with
                | Pcstr_tuple ts ->
                    Vc_tuple (ctor.pcd_name, List.map ts ~f:of_core_type)
                | Pcstr_record fs ->
                    let fs =
                      List.map fs ~f:(fun f ->
                          f.pld_name, of_core_type f.pld_type)
                    in
                    Vc_record (ctor.pcd_name, fs))
          in
          Ts_variant cs
      | Ptype_record fs, _ ->
          let fs =
            List.map fs ~f:(fun f -> f.pld_name, of_core_type f.pld_type)
          in
          Ts_record fs
      | Ptype_open, _ -> not_supported ~loc "open types"
    in
    let params =
      List.map td.ptype_params ~f:(fun (t, _) ->
          match t.ptyp_desc with
          | Ptyp_var name -> { txt = name; loc = t.ptyp_loc }
          | _ -> failwith "type variable is not a variable")
    in
    { name = td.ptype_name; shape; params; loc = td.ptype_loc }

  let te_opaque (n : Longident.t loc) ts =
    ptyp_constr ~loc:n.loc n (List.map ts ~f:fst), Te_opaque (n, ts)

  let decl_to_te_expr decl =
    let loc = decl.loc in
    ptyp_constr ~loc
      { loc; txt = lident decl.name.txt }
      (List.map decl.params ~f:(fun { loc; txt } -> ptyp_var ~loc txt))
end

module Deriving_helper = struct
  let map_loc f a_loc = { a_loc with txt = f a_loc.txt }

  let gen_bindings ~loc prefix n =
    List.split
      (List.init n ~f:(fun i ->
           let id = sprintf "%s_%i" prefix i in
           let patt = ppat_var ~loc { loc; txt = id } in
           let expr = pexp_ident ~loc { loc; txt = lident id } in
           patt, expr))

  let gen_tuple ~loc prefix n =
    let ps, es = gen_bindings ~loc prefix n in
    ps, pexp_tuple ~loc es

  let gen_record ~loc prefix fs =
    let ps, es =
      List.split
        (List.map fs ~f:(fun (n, _t) ->
             let id = sprintf "%s_%s" prefix n.txt in
             let patt = ppat_var ~loc { loc = n.loc; txt = id } in
             let expr =
               pexp_ident ~loc { loc = n.loc; txt = lident id }
             in
             (map_loc lident n, patt), expr))
    in
    let ns, ps = List.split ps in
    ps, pexp_record ~loc (List.combine ns es) None

  let gen_pat_tuple ~loc prefix n =
    let patts, exprs = gen_bindings ~loc prefix n in
    ppat_tuple ~loc patts, exprs

  let gen_pat_list ~loc prefix n =
    let patts, exprs = gen_bindings ~loc prefix n in
    let patt =
      List.fold_left (List.rev patts)
        ~init:[%pat? []]
        ~f:(fun prev patt -> [%pat? [%p patt] :: [%p prev]])
    in
    patt, exprs

  let gen_pat_record ~loc prefix fs =
    let xs =
      List.map fs ~f:(fun (n, _t) ->
          let id = sprintf "%s_%s" prefix n.txt in
          let patt = ppat_var ~loc { loc = n.loc; txt = id } in
          let expr = pexp_ident ~loc { loc = n.loc; txt = lident id } in
          (map_loc lident n, patt), expr)
    in
    ppat_record ~loc (List.map xs ~f:fst) Closed, List.map xs ~f:snd

  let pexp_list ~loc xs =
    List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
        [%expr [%e x] :: [%e xs]])

  let ( --> ) pc_lhs pc_rhs = { pc_lhs; pc_rhs; pc_guard = None }

  let derive_of_label name = function
    | "t" -> name
    | t -> Printf.sprintf "%s_%s" t name

  let derive_of_longident name (lid : Longident.t) =
    match lid with
    | Lident lab -> Longident.Lident (derive_of_label name lab)
    | Ldot (lid, lab) -> Longident.Ldot (lid, derive_of_label name lab)
    | Lapply (_, _) -> failwith "unable to get name of Lapply"

  let ederiver name (lid : Longident.t loc) =
    pexp_ident ~loc:lid.loc (map_loc (derive_of_longident name) lid)
end

type deriver =
  | As_fun of (expression -> expression)
  | As_val of expression

let as_val ~loc deriver x =
  match deriver with As_fun f -> f x | As_val f -> [%expr [%e f] [%e x]]

let as_fun ~loc deriver =
  match deriver with
  | As_fun f -> [%expr fun x -> [%e f [%expr x]]]
  | As_val f -> f

open Repr
open Deriving_helper

class virtual deriving1 =
  object (self)
    method virtual name : string
    method virtual t : loc:location -> label loc -> core_type -> core_type

    method derive_of_tuple
        : loc:location -> type_expr list -> expression -> expression =
      not_supported "tuple types"

    method derive_of_record
        : loc:location ->
          (label loc * type_expr) list ->
          expression ->
          expression =
      not_supported "record types"

    method derive_of_variant
        : loc:location -> variant_case list -> expression -> expression =
      not_supported "variant types"

    method derive_of_polyvariant
        : loc:location ->
          polyvariant_case list ->
          core_type ->
          expression ->
          expression =
      not_supported "variant types"

    method private derive_type_ref' ~loc name n ts =
      let f = ederiver name n in
      let args =
        List.fold_left (List.rev ts) ~init:[] ~f:(fun args a ->
            let a = as_fun ~loc (self#derive_of_type_expr' ~loc a) in
            (Nolabel, a) :: args)
      in
      As_val (pexp_apply ~loc f args)

    method derive_type_ref ~loc name n ts x =
      as_val ~loc (self#derive_type_ref' ~loc name n ts) x

    method private derive_of_type_expr' ~loc =
      function
      | _, Te_tuple ts -> As_fun (self#derive_of_tuple ~loc ts)
      | _, Te_var id -> As_val (ederiver self#name (map_loc lident id))
      | _, Te_opaque (n, ts) -> self#derive_type_ref' self#name ~loc n ts
      | t, Te_polyvariant cs ->
          As_fun (self#derive_of_polyvariant ~loc cs t)

    method derive_of_type_expr ~loc repr x =
      as_val ~loc (self#derive_of_type_expr' ~loc repr) x

    method private derive_type_shape ~loc x =
      function
      | Ts_expr t -> as_val ~loc (self#derive_of_type_expr' ~loc t) x
      | Ts_record fs -> self#derive_of_record ~loc fs x
      | Ts_variant cs -> self#derive_of_variant ~loc cs x

    method derive_type_decl_label name =
      map_loc (derive_of_label self#name) name

    method derive_type_decl ({ name; params; shape; loc } as decl) =
      let expr = self#derive_type_shape ~loc [%expr x] shape in
      let t = Repr.decl_to_te_expr decl in
      let expr = [%expr (fun x -> [%e expr] : [%t self#t ~loc name t])] in
      let expr =
        List.fold_left params ~init:expr ~f:(fun body param ->
            pexp_fun ~loc Nolabel None
              (ppat_var ~loc (map_loc (derive_of_label self#name) param))
              body)
      in
      [
        value_binding ~loc
          ~pat:(ppat_var ~loc (self#derive_type_decl_label name))
          ~expr;
      ]

    method extension
        : loc:location -> path:label -> core_type -> expression =
      fun ~loc:_ ~path:_ ty ->
        let repr = Repr.of_core_type ty in
        let loc = ty.ptyp_loc in
        as_fun ~loc (self#derive_of_type_expr' ~loc repr)

    method generator
        : ctxt:Expansion_context.Deriver.t ->
          rec_flag * type_declaration list ->
          structure =
      fun ~ctxt (_rec_flag, type_decls) ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match List.map type_decls ~f:Repr.of_type_declaration with
        | exception Not_supported (loc, msg) ->
            [ [%stri [%%ocaml.error [%e estring ~loc msg]]] ]
        | reprs ->
            let bindings =
              List.flat_map reprs ~f:(fun decl ->
                  self#derive_type_decl decl)
            in
            [%str
              [@@@ocaml.warning "-39-11-27"]

              [%%i pstr_value ~loc Recursive bindings]]
  end

type deriving =
  | Deriving1 of deriving1
  | Combined of string * deriving * deriving

type derive_of_type_expr =
  loc:location -> Repr.type_expr -> expression -> expression

let deriving_of ~name ~of_t ~error ~derive_of_tuple ~derive_of_record
    ~derive_of_variant ~derive_of_variant_case
    ~derive_of_variant_case_record () =
  let poly_name = sprintf "%s_poly" name in
  let poly =
    object (self)
      inherit deriving1
      method name = name
      method t ~loc _name t = [%type: [%t of_t ~loc] -> [%t t] option]

      method! derive_type_decl_label name =
        map_loc (derive_of_label poly_name) name

      method! derive_of_tuple ~loc =
        derive_of_tuple ~loc self#derive_of_type_expr

      method! derive_of_record ~loc:_ _ _ = assert false
      method! derive_of_variant ~loc:_ _ _ = assert false

      method! derive_of_polyvariant ~loc cs t x =
        let cases =
          List.fold_left (List.rev cs) ~init:[%expr None]
            ~f:(fun next c ->
              match c with
              | Pvc_construct (n, ts) ->
                  let make arg =
                    [%expr Some [%e pexp_variant ~loc:n.loc n.txt arg]]
                  in
                  derive_of_variant_case ~loc self#derive_of_type_expr
                    make n ts next
              | Pvc_inherit (n, ts) ->
                  let x = self#derive_type_ref ~loc poly_name n ts x in
                  [%expr
                    match [%e x] with
                    | Some x -> (Some x :> [%t t] option)
                    | None -> [%e next]])
        in
        derive_of_variant ~loc self#derive_of_type_expr cases x
    end
  in
  Deriving1
    (object (self)
       inherit deriving1 as super
       method name = name
       method t ~loc _name t = [%type: [%t of_t ~loc] -> [%t t]]

       method! derive_of_tuple ~loc =
         derive_of_tuple ~loc self#derive_of_type_expr

       method! derive_of_record ~loc =
         derive_of_record ~loc self#derive_of_type_expr

       method! derive_of_variant ~loc cs x =
         let cases =
           List.fold_left (List.rev cs) ~init:(error ~loc)
             ~f:(fun next c ->
               let make (n : label loc) arg =
                 pexp_construct (map_loc lident n) ~loc:n.loc arg
               in
               match c with
               | Vc_record (n, fs) ->
                   derive_of_variant_case_record ~loc
                     self#derive_of_type_expr (make n) n fs next
               | Vc_tuple (n, ts) ->
                   derive_of_variant_case ~loc self#derive_of_type_expr
                     (make n) n ts next)
         in
         derive_of_variant ~loc self#derive_of_type_expr cases x

       method! derive_of_polyvariant ~loc cs t x =
         let cases =
           List.fold_left (List.rev cs) ~init:(error ~loc)
             ~f:(fun next c ->
               match c with
               | Pvc_construct (n, ts) ->
                   let make arg = pexp_variant ~loc:n.loc n.txt arg in
                   derive_of_variant_case ~loc self#derive_of_type_expr
                     make n ts next
               | Pvc_inherit (n, ts) ->
                   let maybe_e =
                     poly#derive_type_ref ~loc poly_name n ts x
                   in
                   [%expr
                     match [%e maybe_e] with
                     | Some e -> (e :> [%t t])
                     | None -> [%e next]])
         in
         derive_of_variant ~loc self#derive_of_type_expr cases x

       method! derive_type_decl decl =
         match decl.shape with
         | Ts_expr (t, Te_polyvariant _) ->
             let str =
               let { name = decl_name; params; shape = _; loc } = decl in
               let expr =
                 let x = [%expr x] in
                 let init =
                   poly#derive_type_ref ~loc poly_name
                     (map_loc lident decl_name)
                     (List.map params ~f:(fun p ->
                          te_opaque (map_loc lident p) []))
                     x
                 in
                 let init =
                   [%expr
                     (fun x ->
                        match [%e init] with
                        | Some x -> x
                        | None -> [%e error ~loc]
                       : [%t self#t ~loc decl_name t])]
                 in
                 List.fold_left params ~init ~f:(fun body param ->
                     pexp_fun ~loc Nolabel None
                       (ppat_var ~loc
                          (map_loc (derive_of_label name) param))
                       body)
               in
               [
                 value_binding ~loc
                   ~pat:
                     (ppat_var ~loc
                        (map_loc (derive_of_label self#name) decl_name))
                   ~expr;
               ]
             in
             poly#derive_type_decl decl @ str
         | _ -> super#derive_type_decl decl
    end)

let deriving_of_match ~name ~of_t ~error ~derive_of_tuple
    ~derive_of_record ~derive_of_variant_case
    ~derive_of_variant_case_record () =
  let poly_name = sprintf "%s_poly" name in
  let poly =
    object (self)
      inherit deriving1
      method name = name
      method t ~loc _name t = [%type: [%t of_t ~loc] -> [%t t] option]

      method! derive_type_decl_label name =
        map_loc (derive_of_label poly_name) name

      method! derive_of_tuple ~loc =
        derive_of_tuple ~loc self#derive_of_type_expr

      method! derive_of_record ~loc:_ _ _ = assert false
      method! derive_of_variant ~loc:_ _ _ = assert false

      method! derive_of_polyvariant ~loc cs t x =
        let ctors, inherits =
          List.partition_filter_map cs ~f:(function
            | Pvc_construct (n, ts) -> `Left (n, ts)
            | Pvc_inherit (n, ts) -> `Right (n, ts))
        in
        let catch_all =
          [%pat? x]
          --> List.fold_left (List.rev inherits) ~init:[%expr None]
                ~f:(fun next (n, ts) ->
                  let maybe =
                    self#derive_type_ref ~loc poly_name n ts [%expr x]
                  in
                  [%expr
                    match [%e maybe] with
                    | Some x -> (Some x :> [%t t] option)
                    | None -> [%e next]])
        in
        let cases =
          List.fold_left (List.rev ctors) ~init:[ catch_all ]
            ~f:(fun next ((n : label loc), ts) ->
              let make arg =
                [%expr Some [%e pexp_variant ~loc:n.loc n.txt arg]]
              in
              derive_of_variant_case ~loc self#derive_of_type_expr make n
                ts
              :: next)
        in
        pexp_match ~loc x cases
    end
  in
  Deriving1
    (object (self)
       inherit deriving1 as super
       method name = name
       method t ~loc _name t = [%type: [%t of_t ~loc] -> [%t t]]

       method! derive_of_tuple ~loc =
         derive_of_tuple ~loc self#derive_of_type_expr

       method! derive_of_record ~loc =
         derive_of_record ~loc self#derive_of_type_expr

       method! derive_of_variant ~loc cs x =
         let cases =
           List.fold_left (List.rev cs)
             ~init:[ [%pat? _] --> error ~loc ]
             ~f:(fun next c ->
               let make (n : label loc) arg =
                 pexp_construct (map_loc lident n) ~loc:n.loc arg
               in
               match c with
               | Vc_record (n, fs) ->
                   derive_of_variant_case_record ~loc
                     self#derive_of_type_expr (make n) n fs
                   :: next
               | Vc_tuple (n, ts) ->
                   derive_of_variant_case ~loc self#derive_of_type_expr
                     (make n) n ts
                   :: next)
         in
         pexp_match ~loc x cases

       method! derive_of_polyvariant ~loc cs t x =
         let ctors, inherits =
           List.partition_filter_map cs ~f:(function
             | Pvc_construct (n, ts) -> `Left (n, ts)
             | Pvc_inherit (n, ts) -> `Right (n, ts))
         in
         let catch_all =
           [%pat? x]
           --> List.fold_left (List.rev inherits) ~init:(error ~loc)
                 ~f:(fun next (n, ts) ->
                   let maybe =
                     poly#derive_type_ref ~loc poly_name n ts x
                   in
                   [%expr
                     match [%e maybe] with
                     | Some x -> (x :> [%t t])
                     | None -> [%e next]])
         in
         let cases =
           List.fold_left (List.rev ctors) ~init:[ catch_all ]
             ~f:(fun next ((n : label loc), ts) ->
               let make arg = pexp_variant ~loc:n.loc n.txt arg in
               derive_of_variant_case ~loc self#derive_of_type_expr make n
                 ts
               :: next)
         in
         pexp_match ~loc x cases

       method! derive_type_decl decl =
         match decl.shape with
         | Ts_expr (_t, Te_polyvariant _) ->
             let str =
               let { name = decl_name; params; shape = _; loc } = decl in
               let expr =
                 let x = [%expr x] in
                 let init =
                   poly#derive_type_ref ~loc poly_name
                     (map_loc lident decl_name)
                     (List.map params ~f:(fun p ->
                          te_opaque (map_loc lident p) []))
                     x
                 in
                 let init =
                   [%expr
                     (fun x ->
                        match [%e init] with
                        | Some x -> x
                        | None -> [%e error ~loc]
                       : [%t
                           self#t ~loc decl_name
                             (Repr.decl_to_te_expr decl)])]
                 in
                 List.fold_left params ~init ~f:(fun body param ->
                     pexp_fun ~loc Nolabel None
                       (ppat_var ~loc
                          (map_loc (derive_of_label name) param))
                       body)
               in
               [
                 value_binding ~loc
                   ~pat:
                     (ppat_var ~loc
                        (map_loc (derive_of_label self#name) decl_name))
                   ~expr;
               ]
             in
             poly#derive_type_decl decl @ str
         | _ -> super#derive_type_decl decl
    end)

let deriving_to ~name ~t_to ~derive_of_tuple ~derive_of_record
    ~derive_of_variant_case ~derive_of_variant_case_record () =
  Deriving1
    (object (self)
       inherit deriving1
       method name = name
       method t ~loc _name t = [%type: [%t t] -> [%t t_to ~loc]]

       method! derive_of_tuple ~loc ts x =
         let n = List.length ts in
         let p, es = gen_pat_tuple ~loc "x" n in
         pexp_match ~loc x
           [ p --> derive_of_tuple ~loc self#derive_of_type_expr ts es ]

       method! derive_of_record ~loc fs x =
         let p, es = gen_pat_record ~loc "x" fs in
         pexp_match ~loc x
           [ p --> derive_of_record ~loc self#derive_of_type_expr fs es ]

       method! derive_of_variant ~loc cs x =
         let ctor_pat (n : label loc) pat =
           ppat_construct ~loc:n.loc (map_loc lident n) pat
         in
         pexp_match ~loc x
           (List.map cs ~f:(function
             | Vc_record (n, fs) ->
                 let p, es = gen_pat_record ~loc "x" fs in
                 ctor_pat n (Some p)
                 --> derive_of_variant_case_record ~loc
                       self#derive_of_type_expr n fs es
             | Vc_tuple (n, ts) ->
                 let arity = List.length ts in
                 let p, es = gen_pat_tuple ~loc "x" arity in
                 ctor_pat n (if arity = 0 then None else Some p)
                 --> derive_of_variant_case ~loc self#derive_of_type_expr
                       n ts es))

       method! derive_of_polyvariant ~loc cs _t x =
         let cases =
           List.map cs ~f:(function
             | Pvc_construct (n, []) ->
                 ppat_variant ~loc n.txt None
                 --> derive_of_variant_case ~loc self#derive_of_type_expr
                       n [] []
             | Pvc_construct (n, ts) ->
                 let ps, es = gen_pat_tuple ~loc "x" (List.length ts) in
                 ppat_variant ~loc n.txt (Some ps)
                 --> derive_of_variant_case ~loc self#derive_of_type_expr
                       n ts es
             | Pvc_inherit (n, ts) ->
                 [%pat? [%p ppat_type ~loc n] as x]
                 --> self#derive_of_type_expr ~loc (te_opaque n ts)
                       [%expr x])
         in
         pexp_match ~loc x cases
    end)

let combined ~name a b = Combined (name, a, b)

let register ?deps = function
  | Deriving1 deriving ->
      Deriving.add deriving#name
        ~str_type_decl:
          (Deriving.Generator.V2.make ?deps Deriving.Args.empty
             deriving#generator)
        ~extension:deriving#extension
  | Combined (name, _, _) as d ->
      let rec collect = function
        | Combined (_, a, b) -> collect b @ collect a
        | Deriving1 a -> [ a ]
      in
      let ds = collect d in
      let generator ~ctxt bindings =
        List.fold_left ds ~init:[] ~f:(fun str d ->
            d#generator ~ctxt bindings @ str)
      in
      Deriving.add name
        ~str_type_decl:
          (Deriving.Generator.V2.make ?deps Deriving.Args.empty generator)
