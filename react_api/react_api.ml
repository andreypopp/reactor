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

  type html_element = ?className:string -> children -> element

  val h : string -> html_element
  (** [h tagname ?className children] renders an HTML element [tagname]. *)

  val html : html_element
  val body : html_element
  val head : html_element
  val title : html_element
  val div : html_element
  val span : html_element
  val li : html_element
  val ul : html_element
  val ol : html_element
  val a : html_element
  val h1 : html_element
  val h2 : html_element
  val h3 : html_element

  val suspense : ?fallback:children -> children -> element
  (** Renders a React Suspense boundary. *)

  val use_effect : (unit -> unit -> unit) -> 'a array -> unit
  val use_effect' : (unit -> unit) -> 'a array -> unit

  type 'a promise

  val use : 'a promise -> 'a
end
