module type PROMISE = sig
  type 'a promise

  val sleep : float -> unit promise
end

module type REACT = sig
  type element
  type children = element array

  val null : element
  (** [null] renders nothing *)

  val text : string -> element
  (** [text s] renders string [s]. *)

  val suspense : ?fallback:children -> children -> element
  (** Renders a React Suspense boundary. *)

  val use_effect : (unit -> unit -> unit) -> 'a array -> unit
  val use_effect' : (unit -> unit) -> 'a array -> unit

  type 'a promise

  val use : 'a promise -> 'a
end
