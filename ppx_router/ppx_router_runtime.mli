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
