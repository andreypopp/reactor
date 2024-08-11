(** Tag requests to invalidate cached responses. *)
module Tag : sig
  type t

  val make : string -> t
end

module Make (Route : sig
  type 'a t

  val href : 'a t -> string
  val body : 'a t -> Json.t option
  val encode_response : 'a t -> 'a -> Json.t
  val handle : 'a t -> 'a Promise.t
  val witness : 'a t -> 'a Ppx_deriving_router_runtime_lib.Witness.t
end) : sig
  type 'a route = 'a Route.t

  val fetch : ?tags:Tag.t list -> 'a route -> 'a Promise.t
  (** [fetch route] runs [Route.handle route] but caches the results within the
      same [Runner.ctx]. *)

  val run : 'a route -> 'a Promise.t
  (** [run route] runs [Route.handle route]. *)

  val handle : 'a route -> 'a Promise.t
  (** [handle route] is equivalent to [Route.handle route] *)
end

module Context : sig
  type t

  val create : unit -> t
  (** Create a new context. *)

  val wait : t -> unit Lwt.t
  (** Wait for all running fetches finish. *)

  type batch
  (** A batch of requests. *)

  val batch_to_html : t -> batch -> Htmlgen.t Lwt.t
  (** [fetch_to_html fetch] converts a fetch request to an HTML representation. *)

  val with_ctx : t -> (unit -> 'a) -> 'a * batch
  (** [with_ctx ctx f] runs [f] in the context [ctx]. All fetch requests made
      during the execution of [f] are recorded and returned as a list. *)

  val with_ctx_async :
    t -> (unit -> 'a Promise.t) -> ('a * batch) Promise.t
  (** [with_ctx_async ctx f] is similar to [with_ctx] but [f] can be async. *)
end
