(** React interfaces *)

module type PROMISE = sig
  type 'a promise
  (** A value which is will be eventually computed. *)

  val sleep : float -> unit promise
end

module type REACT = sig
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
  val use_effect : (unit -> unit -> unit) -> _ array -> unit
  val use_effect' : (unit -> unit) -> _ array -> unit
  val use_memo : (unit -> 'a) -> _ array -> 'a
  val use_callback : ('a -> 'b) -> _ array -> 'a -> 'b
  val start_transition : (unit -> unit) -> unit

  type 'a promise

  val use : 'a promise -> 'a
end
