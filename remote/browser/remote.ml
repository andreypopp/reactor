module Witness = Ppx_deriving_router_runtime.Witness
module Promise = Realm.Promise

type 'a req = {
  path : string;
  input : string Lazy.t;
  decode_response : Fetch.Response.t -> 'a Promise.t;
  witness : 'a Witness.t;
}

module Cache : sig
  val cached : 'a req -> (unit -> string Promise.t) -> 'a Promise.t
  val invalidate : 'a req -> unit
end = struct
  type record = {
    json : string Promise.t;
    mutable data : cached_data Js.nullable;
  }
  (** Cache record. We split into json and data fields b/c SSR payload
      contains only json data, so we should be able to decode such "partial"
      cache into a properly typed value. *)

  and cached_data =
    | Cached_data : 'a Witness.t * 'a Promise.t -> cached_data

  external new_Response : string -> Fetch.response = "Response"
  [@@mel.new]

  let cached_data req (data : string Promise.t) =
    Promise.(
      let data =
        let* data = data in
        let response = new_Response data in
        req.decode_response response
      in
      data, Cached_data (req.witness, data))

  type t = record Js.Dict.t Js.Dict.t

  external window : t Js.Dict.t = "window"
  external t : t Js.Undefined.t = "window.__Remote_cache"

  let lookup req =
    let cache =
      match Js.Undefined.toOption t with
      | None ->
          let t = Js.Dict.empty () in
          Js.Dict.set window "__Remote_cache" t;
          t
      | Some t -> t
    in
    let cache =
      match Js.Dict.get cache req.path with
      | None ->
          let cache' = Js.Dict.empty () in
          Js.Dict.set cache req.path cache';
          cache'
      | Some cache' -> cache'
    in
    let key = Lazy.force req.input in
    cache, key, Js.Dict.get cache key

  let cached (type a) (req : a req) (f : unit -> string Promise.t) :
      a Promise.t =
    let cache, key, record = lookup req in
    let fresh () =
      let json = f () in
      let data, cached_data = cached_data req json in
      Js.Dict.set cache key
        { json; data = Js.Nullable.return cached_data };
      data
    in
    match record with
    | None -> fresh ()
    | Some record -> (
        match Js.Nullable.toOption record.data with
        | None ->
            let data, cached_data = cached_data req record.json in
            record.data <- Js.Nullable.return cached_data;
            data
        | Some (Cached_data (key, data)) -> (
            match Witness.equal req.witness key with
            | None ->
                (* This shouldn't happen as requests are identified by
                   path and we already found a corresponding cache record *)
                assert false
            | Some Eq -> data))

  let invalidate req =
    match Js.Undefined.toOption t with
    | None -> ()
    | Some t -> (
        match Js.Dict.get t req.path with
        | None -> ()
        | Some t ->
            Js.Dict.unsafeDeleteKey (Obj.magic t) (Lazy.force req.input)
            [@u])
end

module Make (Route : sig
  type 'a t

  val http_method : 'a t -> [ `GET | `POST | `PUT | `DELETE ]
  val href : 'a t -> string
  val body : 'a t -> Ppx_deriving_json_runtime.t option
  val decode_response : 'a t -> Fetch.Response.t -> 'a Js.Promise.t
  val witness : 'a t -> 'a Witness.t
end) =
struct
  module F = Ppx_deriving_router_runtime.Make_fetch (Route)

  let run route = F.fetch route

  let to_req route =
    {
      witness = Route.witness route;
      path = Route.href route;
      input =
        lazy
          (Js.Json.stringify
             (match Route.body route with
             | None -> Js.Json.null
             | Some body -> body));
      decode_response = Route.decode_response route;
    }

  let fetch route =
    Cache.cached (to_req route) @@ fun () ->
    Promise.(
      let* response = F.fetch' route in
      match Fetch.Response.ok response with
      | true -> Fetch.Response.text response
      | false -> failwith "got non 200")

  let invalidate route = Cache.invalidate (to_req route)
end
