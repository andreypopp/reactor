(** React interfaces *)

module type REACT = sig
  type dom_element
  type 'a nullable
  type element
  type children = element array

  val array : children -> element
  val list : element list -> element
  val null : element
  val string : string -> element
  val int : int -> element
  val float : float -> element
  val stringf : ('a, unit, string, element) format4 -> 'a
  val useState : (unit -> 'state) -> 'state * (('state -> 'state) -> unit)
  val useEffect : (unit -> (unit -> unit) option) -> unit
  val useEffect1 : (unit -> (unit -> unit) option) -> 'a array -> unit
  val startTransition : (unit -> unit) -> unit

  module Context : sig
    type 'a t

    val provider :
      'a t ->
      ?key:string ->
      value:'a ->
      children:element ->
      unit ->
      element
  end

  val createContext : 'a -> 'a Context.t
  val useContext : 'a Context.t -> 'a

  module Suspense : sig
    val make :
      ?key:string ->
      ?fallback:element ->
      children:element ->
      unit ->
      element
  end

  type 'value ref = { mutable current : 'value }

  val useRef : 'value -> 'value ref

  type 'a promise

  val use : 'a promise -> 'a
end
