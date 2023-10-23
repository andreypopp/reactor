(** Combinators for HTML generation. *)

(** {1 HTML model} *)

(** {2 HTML attributes} *)

type attrs = (string * attr_value) list
(** A list of HTML attributes. *)

and attr_value =
  [ `String of string | `Bool of bool | `Int of int | `Float of float ]

val s : string -> attr_value
val b : bool -> attr_value
val i : int -> attr_value

(** {2 HTML elements} *)

type t
(** An HTML element. *)

val node : string -> attrs -> t list -> t
(** [node tag attrs children] produces an HTML tag with specified attributes and
    children elements. 

    e.g 
    {v node "div" ["className", s "block"] [ text "hello" ] v}
    renders into
    {v <div class="block">hello</div> v}

    *)

val text : string -> t
(** [text s] renders [s] string, escaping its content. *)

val splice : ?sep:string -> t list -> t
(** [splice ~sep xs] concats [xs] elements together using [sep] separator (by
    default it is empty) *)

val empty : t
(** [empty] renders nothing *)

val unsafe_raw : string -> t
(** Unsafely (without any escaping) embed a string into HTML. Never use this
    untrusted input. *)

val unsafe_rawf : ('a, unit, string, t) format4 -> 'a
(** Same as [unsafe_raw] but allows to use a format string. Never use this
    untrusted input. *)

(** {1 HTML Rendering} *)

val to_string : t -> string
(** Render HTML to string. *)

(** {1 Various utilities} *)

val single_quote_escape : string -> string
(** Escape ['] single quote as HTML entity. This can be used to embed arbitray
    values within the ['] single quoted HTML attribute. Use with care. *)
