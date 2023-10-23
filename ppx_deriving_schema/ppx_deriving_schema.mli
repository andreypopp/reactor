open Ppxlib

module Repr : sig
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

  and type_expr =
    | Te_opaque of longident loc * type_expr list
    | Te_var of label loc
    | Te_tuple of type_expr list
    | Te_polyvariant of polyvariant_case list

  and variant_case =
    | Vc_tuple of label loc * type_expr list
    | Vc_record of label loc * (label loc * type_expr) list

  and polyvariant_case =
    | Pvc_construct of label loc * type_expr list
    | Pvc_inherit of Longident.t loc * type_expr list

  val of_core_type : core_type -> type_expr
  val of_type_declaration : type_declaration -> type_decl
end

module Deriving1 : functor
  (_ : sig
     val name : string
     val t : loc:location -> core_type -> core_type

     val derive_of_tuple :
       loc:location ->
       (loc:location -> Repr.type_expr -> expression -> expression) ->
       Repr.type_expr list ->
       expression ->
       expression

     val derive_of_record :
       loc:location ->
       (loc:location -> Repr.type_expr -> expression -> expression) ->
       (label loc * Repr.type_expr) list ->
       expression ->
       expression

     val derive_of_variant :
       loc:location ->
       (loc:location -> Repr.type_expr -> expression -> expression) ->
       Repr.variant_case list ->
       expression ->
       expression
   end)
  -> sig
  val register : unit -> unit
end

module Deriving_helper : sig
  val gen_pat_tuple :
    loc:location -> string -> int -> pattern * expression list
  (** [let patt, exprs = gen_pat_tuple ~loc prefix n in ...]
      generates a pattern to match a tuple of size [n] and a list of expressions
      [exprs] to refer to names bound in this pattern. *)

  val gen_pat_list :
    loc:location -> string -> int -> pattern * expression list
  (** [let patt, exprs = gen_pat_list ~loc prefix n in ...]
      generates a pattern to match a list of size [n] and a list of expressions
      [exprs] to refer to names bound in this pattern. *)

  val gen_pat_record :
    loc:location ->
    string ->
    (label loc * 'a) list ->
    pattern * expression list
  (** [let patt, exprs = gen_pat_record ~loc prefix fs in ...]
      generates a pattern to match record with fields [fs] and a list of expressions
      [exprs] to refer to names bound in this pattern. *)

  val to_lident : label loc -> longident loc
  val ( --> ) : pattern -> expression -> case
end
