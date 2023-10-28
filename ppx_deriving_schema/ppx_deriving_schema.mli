open Ppxlib

(** Simplified type expression / declaration representation. *)
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

  and type_expr = core_type * type_expr'

  and type_expr' =
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
  val te_opaque : longident loc -> type_expr list -> type_expr
end

(** define an encoder/serializer-like deriver *)
class virtual derive_to : object
  method virtual name : string
  (** name of the deriver *)

  method virtual t_to : loc:location -> core_type
  (** type the deriver encodes the type to *)

  method virtual derive_of_tuple :
    loc:location -> Repr.type_expr list -> expression list -> expression
  (** define how to encode a tuple *)

  method virtual derive_of_record :
    loc:location ->
    (label loc * Repr.type_expr) list ->
    expression list ->
    expression
  (** define how to encode a record *)

  method virtual derive_of_variant_case :
    loc:location ->
    label loc ->
    Repr.type_expr list ->
    expression list ->
    expression
  (** define how to encode a single variant case with a tuple argument *)

  method virtual derive_of_variant_case_record :
    loc:location ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    expression list ->
    expression
  (** define how to encode a single variant case with a record argument *)

  method derive_type_expr :
    loc:location -> Repr.type_expr -> expression -> expression
  (** get an expression to encode a type expression, use this for recursive
      calls *)

  method extension : loc:location -> path:label -> core_type -> expression

  method generator :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    structure
end

(** define a decoder/deserializer-like deriver *)
class virtual deriving_of : object
  method virtual name : string
  (** name of the deriver *)

  method virtual of_t : loc:location -> core_type
  (** type the deriver decodes the type from *)

  method virtual error : loc:location -> expression
  (** an expression which represent a decoder error *)

  method virtual derive_of_tuple :
    loc:location -> Repr.type_expr list -> expression -> expression
  (** define how to decode a tuple *)

  method virtual derive_of_record :
    loc:location ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression
  (** define how to decode a record *)

  method virtual derive_of_variant_parse :
    loc:location -> expression -> expression -> expression

  method virtual derive_of_variant_case :
    loc:location ->
    (expression option -> expression) ->
    label loc ->
    Repr.type_expr list ->
    expression ->
    expression
  (** define how to encode a single variant case with a tuple argument *)

  method virtual derive_of_variant_case_record :
    loc:location ->
    (expression option -> expression) ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression
  (** define how to encode a single variant case with a record argument *)

  method derive_type_expr :
    loc:location -> Repr.type_expr -> expression -> expression
  (** get an expression to encode a type expression, use this for recursive
      calls *)

  method extension : loc:location -> path:label -> core_type -> expression

  method generator :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    structure
end

(** define a decoder/deserializer-like deriver (via pattern matching) *)
class virtual deriving_of_cases : object
  method virtual name : string
  (** name of the deriver *)

  method virtual of_t : loc:location -> core_type
  (** type the deriver decodes the type from *)

  method virtual error : loc:location -> expression
  (** an expression which represent a decoder error *)

  method virtual derive_of_tuple :
    loc:location -> Repr.type_expr list -> expression -> expression
  (** define how to decode a tuple *)

  method virtual derive_of_record :
    loc:location ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression
  (** define how to decode a record *)

  method virtual derive_of_variant_case :
    loc:location ->
    (expression option -> expression) ->
    label loc ->
    Repr.type_expr list ->
    case
  (** define how to encode a single variant case with a tuple argument *)

  method virtual derive_of_variant_case_record :
    loc:location ->
    (expression option -> expression) ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    case
  (** define how to encode a single variant case with a record argument *)

  method derive_type_expr :
    loc:location -> Repr.type_expr -> expression -> expression
  (** get an expression to encode a type expression, use this for recursive
      calls *)

  method extension : loc:location -> path:label -> core_type -> expression

  method generator :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    structure
end

(** define a generic deriver *)
class virtual deriving1 : object
  method virtual name : string
  (** name of the deriver *)

  method binding_name : string
  (** name of the binding produced by the deriver (defaults to [name] but can
      be overriden to customize "shallow" derivers) *)

  method virtual t : loc:location -> core_type -> core_type
  (** produce a type expression for the deriver *)

  method virtual derive_of_tuple :
    loc:location -> Repr.type_expr list -> expression -> expression

  method virtual derive_of_record :
    loc:location ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression

  method virtual derive_of_variant :
    loc:location -> Repr.variant_case list -> expression -> expression

  method virtual derive_of_polyvariant :
    loc:location ->
    Repr.polyvariant_case list ->
    core_type ->
    expression ->
    expression

  method derive_type_expr :
    loc:location -> Repr.type_expr -> expression -> expression

  method derive_type_decl : Repr.type_decl -> value_binding list

  method derive_type_ref :
    loc:location ->
    label ->
    longident loc ->
    Repr.type_expr list ->
    expression ->
    expression

  method extension : loc:location -> path:label -> core_type -> expression

  method generator :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    structure
end

val register :
  ?deps:Deriving.t list ->
  < extension : loc:location -> path:label -> core_type -> expression
  ; generator :
      ctxt:Expansion_context.Deriver.t ->
      rec_flag * type_declaration list ->
      Ppxlib__Import.structure
  ; name : label
  ; .. > ->
  Deriving.t

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
