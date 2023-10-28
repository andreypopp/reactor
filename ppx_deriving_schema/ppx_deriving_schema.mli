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

type deriving
(** A deriving definition. *)

type derive_of_type_expr =
  loc:location -> Repr.type_expr -> expression -> expression

val deriving_to :
  name:label ->
  t_to:(loc:location -> core_type) ->
  derive_of_tuple:
    (loc:location ->
    derive_of_type_expr ->
    Repr.type_expr list ->
    expression list ->
    expression) ->
  derive_of_record:
    (loc:location ->
    derive_of_type_expr ->
    (label loc * Repr.type_expr) list ->
    expression list ->
    expression) ->
  derive_of_variant_case:
    (loc:location ->
    derive_of_type_expr ->
    label loc ->
    Repr.type_expr list ->
    expression list ->
    expression) ->
  derive_of_variant_case_record:
    (loc:location ->
    derive_of_type_expr ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    expression list ->
    expression) ->
  unit ->
  deriving
(** Define an encoder-like deriving. *)

val deriving_of :
  name:label ->
  of_t:(loc:location -> core_type) ->
  error:(loc:location -> expression) ->
  derive_of_tuple:
    (loc:location ->
    derive_of_type_expr ->
    Repr.type_expr list ->
    expression ->
    expression) ->
  derive_of_record:
    (loc:location ->
    derive_of_type_expr ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression) ->
  derive_of_variant:
    (loc:location ->
    derive_of_type_expr ->
    expression ->
    expression ->
    expression) ->
  derive_of_variant_case:
    (loc:location ->
    derive_of_type_expr ->
    (expression option -> expression) ->
    label loc ->
    Repr.type_expr list ->
    expression ->
    expression) ->
  derive_of_variant_case_record:
    (loc:location ->
    derive_of_type_expr ->
    (expression option -> expression) ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression) ->
  unit ->
  deriving
(** Define an decoder-like deriving. *)

val deriving_of_match :
  name:label ->
  of_t:(loc:location -> core_type) ->
  error:(loc:location -> expression) ->
  derive_of_tuple:
    (loc:location ->
    derive_of_type_expr ->
    Repr.type_expr list ->
    expression ->
    expression) ->
  derive_of_record:
    (loc:location ->
    derive_of_type_expr ->
    (label loc * Repr.type_expr) list ->
    expression ->
    expression) ->
  derive_of_variant_case:
    (loc:location ->
    derive_of_type_expr ->
    (expression option -> expression) ->
    label loc ->
    Repr.type_expr list ->
    case) ->
  derive_of_variant_case_record:
    (loc:location ->
    derive_of_type_expr ->
    (expression option -> expression) ->
    label loc ->
    (label loc * Repr.type_expr) list ->
    case) ->
  unit ->
  deriving
(** Define an decoder-like deriving via pattern matching. *)

val register : ?deps:Deriving.t list -> deriving -> Deriving.t
(** Register a deriving. *)

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
  (** A shortcut to define a pattern matching case. *)
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

  method derive_of_type_expr :
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
