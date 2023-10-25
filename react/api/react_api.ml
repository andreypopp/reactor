(** React interfaces *)

module type REACT = sig
  type dom_element
  type 'a nullable
  type element
  type children = element array

  val array : children -> element
  val list : element list -> element

  val null : element
  (** [null] renders nothing *)

  val string : string -> element
  (** [string s] renders string [s]. *)

  val stringf : ('a, unit, string, element) format4 -> 'a
  val useState : (unit -> 'state) -> 'state * (('state -> 'state) -> unit)
  val useEffect : (unit -> (unit -> unit) option) -> unit
  val useEffect1 : (unit -> (unit -> unit) option) -> 'a array -> unit
  val startTransition : (unit -> unit) -> unit

  type 'value ref = { mutable current : 'value }

  val useRef : 'value -> 'value ref

  type 'a promise

  val use : 'a promise -> 'a
end
