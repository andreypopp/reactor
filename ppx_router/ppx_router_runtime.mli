type 'handle route = {
  method_ : [ `GET | `POST | `PUT | `DELETE ];
  path : string;
  handle : 'handle;
}

val to_route :
  (('a -> 'a) -> 'b -> Dream.handler) route -> 'b -> Dream.route

val encode_path : Buffer.t -> string -> unit
val encode_query_key : Buffer.t -> string -> unit
val encode_query_value : Buffer.t -> string -> unit

module Types : sig
  val string_to_url : string -> string
  val string_of_url : string -> string option
  val int_to_url : int -> string
  val int_of_url : string -> int option
  val bool_to_url : bool -> string
  val bool_of_url : string -> bool option
end
