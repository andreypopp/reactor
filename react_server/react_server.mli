type json = Yojson.Safe.t

(** DSL for constructing UI elements, this module is intended to be openned. *)
module React : sig
  include React_api.REACT with type 'a promise = 'a Lwt.t

  val textf : ('a, unit, string, element) format4 -> 'a
  (** Like [text] but allows to use printf formatting. *)

  val thunk : (unit -> element) -> element
  (** [thunk f props children] renders an element tree produced by [f props
      children]. *)

  val async_thunk : (unit -> element promise) -> element
  (** [async_thunk f props children] works the same as [thunk f props children]
      but is asynchronous. *)

  type client_props = (string * [ json | `Element of element ]) list

  val client_thunk :
    ?import_name:string -> string -> client_props -> element -> element
  (** This instructs to render a client components in browser which is
      implemented in JavaScript. *)

  module Html_prop : sig
    type prop

    val className : string -> prop
  end

  type unsafe_html = { __html : string }

  type html_children =
    | Html_children of children
    | Html_children_raw of unsafe_html

  val unsafe_create_html_element :
    string -> Html_prop.prop list -> html_children option -> element

  val use_effect : (unit -> unit -> unit) -> 'a array -> unit
  [@@alert
    browser_only
      "React.use_effect is only available for client side components"]

  val use_effect' : (unit -> unit) -> 'a array -> unit
  [@@alert
    browser_only
      "React.use_effect' is only available for client side components"]

  val use : 'a promise -> 'a
  [@@alert
    browser_only
      "React.use is only available for client side components, use async \
       components on server instead"]

  exception Browser_only
end

module React_browser : sig
  module Promise : React_api.PROMISE with type 'a promise = 'a Lwt.t

  module React :
    React_api.REACT
      with type element = React.element
       and type 'a promise = 'a Promise.promise
end

val render_to_model :
  React.element -> (string -> unit Lwt.t) -> unit Lwt.t

type html_rendering =
  | Html_rendering_done of { html : Html.t }
  | Html_rendering_async of {
      html_shell : Html.t;
      html_iter : (Html.t -> unit Lwt.t) -> unit Lwt.t;
    }

val render_to_html : React.element -> html_rendering Lwt.t

module Html : module type of Html
