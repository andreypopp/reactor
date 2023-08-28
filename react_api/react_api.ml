(** React interfaces *)

module type REACT = sig
  type dom_element
  type 'a nullable
  type element
  type children = element array

  val null : element
  (** [null] renders nothing *)

  val text : string -> element
  (** [text s] renders string [s]. *)

  val textf : ('a, unit, string, element) format4 -> 'a

  val suspense : ?key:string -> ?fallback:children -> children -> element
  (** Renders a React Suspense boundary. *)

  val use_state : (unit -> 'a) -> 'a * (('a -> 'a) -> unit)
  val use_effect0 : (unit -> unit -> unit) -> unit
  val use_effect1 : 'a -> (unit -> unit -> unit) -> unit
  val use_effect2 : 'a -> 'b -> (unit -> unit -> unit) -> unit
  val use_effect0' : (unit -> unit) -> unit
  val use_effect1' : 'a -> (unit -> unit) -> unit
  val use_effect2' : 'a -> 'b -> (unit -> unit) -> unit
  val use_layout_effect0 : (unit -> unit -> unit) -> unit
  val use_layout_effect1 : 'a -> (unit -> unit -> unit) -> unit
  val use_layout_effect2 : 'a -> 'b -> (unit -> unit -> unit) -> unit
  val use_layout_effect0' : (unit -> unit) -> unit
  val use_layout_effect1' : 'a -> (unit -> unit) -> unit
  val use_layout_effect2' : 'a -> 'b -> (unit -> unit) -> unit
  val use_memo0 : (unit -> 'v) -> 'v
  val use_memo1 : 'a -> (unit -> 'v) -> 'v
  val use_memo2 : 'a -> 'b -> (unit -> 'v) -> 'v
  val use_callback0 : ('v -> 'w) -> 'v -> 'w
  val use_callback1 : 'a -> ('v -> 'w) -> 'v -> 'w
  val use_callback2 : 'a -> 'b -> ('v -> 'w) -> 'v -> 'w
  val start_transition : (unit -> unit) -> unit

  type 'a ref

  val deref : 'a ref -> 'a option
  val use_ref : unit -> 'a ref * ('a option -> unit)

  val use_dom_ref :
    unit -> dom_element ref * (dom_element nullable -> unit)

  type 'a promise

  val use : 'a promise -> 'a
end
