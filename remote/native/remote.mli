module Runner : sig
  type ctx

  val create : unit -> ctx
  val wait : ctx -> unit Lwt.t

  type running =
    | Running : {
        path : string;
        input : Json.t;
        yojson_of_output : 'a -> Json.t;
        promise : 'a Promise.t;
      }
        -> running

  val with_ctx : ctx -> (unit -> 'a) -> 'a * running list

  val with_ctx_async :
    ctx -> (unit -> 'a Promise.t) -> ('a * running list) Promise.t
end

module Make (Route : sig
  type 'a t

  val href : 'a t -> string
  val body : 'a t -> Json.t option
  val encode_response : 'a t -> 'a -> Json.t
  val handle : 'a t -> 'a Promise.t
  val witness : 'a t -> 'a Ppx_deriving_router_runtime.Witness.t
end) : sig
  type 'a route = 'a Route.t

  val fetch : 'a route -> 'a Promise.t
  (** [fetch route] runs [Route.handle route] but caches the results within the
      same [Runner.ctx]. *)

  val run : 'a route -> 'a Promise.t
  (** [run route] runs [Route.handle route]. *)

  val handle : 'a route -> 'a Promise.t
  (** [handle route] is equivalent to [Route.handle route] *)
end
