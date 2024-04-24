type json = Json.t

module Make (Route : sig
  type 'a t

  val http_method : 'a t -> [ `DELETE | `GET | `POST | `PUT ]
  val href : 'a t -> string
  val body : 'a t -> Ppx_deriving_json_runtime.t option
  val decode_response : 'a t -> Fetch.response -> 'a Promise.t
  val witness : 'a t -> 'a Ppx_deriving_router_runtime.Witness.t
end) : sig
  val run : 'a Route.t -> 'a Promise.t
  val fetch : 'a Route.t -> 'a Promise.t
  val invalidate : 'a Route.t -> unit
end
