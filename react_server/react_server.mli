type json = Yojson.Safe.t

(** DSL for constructing UI elements, this module is intended to be openned. *)
module React : sig
  type element
  (** An abstract UI specification, see [React_element] below for how to
    construct values of such type. *)

  type children = element array
  (** Just a convenience, should be replaced by JSX syntax. *)

  val null : element
  (** An element which renders nothing. *)

  val many : children -> element
  (** An element which renders multiple elements. *)

  val text : string -> element
  (** An element which renders [text]. *)

  val textf : ('a, unit, string, element) format4 -> 'a
  (** Like [text] but allows to use printf formatting. *)

  type html_element = ?className:string -> children -> element

  val html : string -> html_element
  (** Render HTML element. *)

  val div : html_element
  val span : html_element
  val li : html_element
  val ul : html_element
  val ol : html_element
  val a : html_element
  val h1 : html_element
  val h2 : html_element
  val h3 : html_element

  val thunk : (unit -> element) -> element
  (** [thunk f props children] renders an element tree produced by [f props
      children]. *)

  val async_thunk : (unit -> element Lwt.t) -> element
  (** [async_thunk f props children] works the same as [thunk f props children]
      but is asynchronous. *)

  val suspense : children -> element
  (** Renders a React Suspense boundary. *)

  type client_props = (string * [ json | `Element of element ]) list

  val client_thunk :
    ?import_name:string -> string -> client_props -> element -> element
  (** This instructs to render a client components in browser which is
      implemented in JavaScript. *)
end

val render :
  ?scripts:string list ->
  ?links:string list ->
  (Dream.request -> React.element) ->
  Dream.handler
(** Serve React Server Component. *)

val esbuild : ?sourcemap:bool -> string list -> Dream.handler
(** Serve esbuild bundle. 

    This requires [esbuild] executable to be on your [$PATH].
 *)
