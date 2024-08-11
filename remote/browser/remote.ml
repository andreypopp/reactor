module Witness = Ppx_deriving_router_runtime.Witness
module Promise = Realm.Promise

module Tag : sig
  type t

  val make : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let make x = x
  let to_string x = x
end

type 'a req = {
  path : string;
  input : string Lazy.t;
  decode_response : Fetch.Response.t -> 'a Promise.t;
  witness : 'a Witness.t;
  tags : Tag.t list;
}

module Cache : sig
  val cached : 'a req -> (unit -> string Promise.t) -> 'a Promise.t
  val invalidate : 'a req -> unit
  val invalidate_tag : Tag.t -> unit
end = struct
  module Record : sig
    type t = {
      mutable json : string Promise.t Js.nullable;
      mutable data : cached_data Js.nullable;
      mutable tags : string list;
    }

    and cached_data =
      | Cached_data : 'a Witness.t * 'a Promise.t -> cached_data

    val invalidate : t -> unit
  end = struct
    type t = {
      mutable json : string Promise.t Js.nullable;
      mutable data : cached_data Js.nullable;
      mutable tags : string list;
    }
    (** Cache record. We split into json and data fields b/c SSR payload
      contains only json data, so we should be able to decode such "partial"
      cache into a properly typed value. *)

    and cached_data =
      | Cached_data : 'a Witness.t * 'a Promise.t -> cached_data

    let invalidate t =
      t.json <- Js.Nullable.null;
      t.data <- Js.Nullable.null
  end

  external new_Response : string -> Fetch.response = "Response"
  [@@mel.new]

  let cached_data req (data : string Promise.t) =
    Promise.(
      let data =
        let* data = data in
        let response = new_Response data in
        req.decode_response response
      in
      data, Record.Cached_data (req.witness, data))

  type window

  external window : window = "window"

  module By_req : sig
    type t = Record.t Js.Dict.t Js.Dict.t

    val find : t -> _ req -> Record.t Js.dict * string * Record.t option
    val invalidate : t -> _ req -> unit
    val instance : t Lazy.t
    val iter : f:(Record.t -> unit) -> t -> unit
  end = struct
    type t = Record.t Js.Dict.t Js.Dict.t

    let find t req =
      let t =
        match Js.Dict.get t req.path with
        | None ->
            let cache' = Js.Dict.empty () in
            Js.Dict.set t req.path cache';
            cache'
        | Some cache' -> cache'
      in
      let k = Lazy.force req.input in
      let v = Js.Dict.get t k in
      t, k, v

    let invalidate t req =
      match Js.Dict.get t req.path with
      | None -> ()
      | Some t -> (
          match Js.Dict.get t (Lazy.force req.input) with
          | None -> ()
          | Some record -> Record.invalidate record)

    external instance : t Js.Undefined.t = "window.__Remote_cache"

    let instance =
      lazy
        (match Js.Undefined.toOption instance with
        | None ->
            let t = Js.Dict.empty () in
            Js.Dict.set
              (Obj.magic window : t Js.Dict.t)
              "__Remote_cache" t;
            t
        | Some t -> t)

    let iter ~f t =
      Array.iter
        (fun k ->
          let t = Js.Dict.unsafeGet t k in
          Array.iter
            (fun k ->
              let v = Js.Dict.unsafeGet t k in
              f v)
            (Js.Dict.keys t))
        (Js.Dict.keys t)
  end

  module By_tag : sig
    val set : prev_tags:string list -> 'a req -> Record.t -> unit
    val invalidate : Tag.t -> unit
  end = struct
    type t = By_req.t Js.Dict.t

    external instance : t Js.Undefined.t = "window.__Remote_cache_by_tag"

    let instance =
      lazy
        (match Js.Undefined.toOption instance with
        | None ->
            let t = Js.Dict.empty () in
            Js.Dict.set
              (Obj.magic window : t Js.Dict.t)
              "__Remote_cache_by_tag" t;
            t
        | Some t -> t)

    let for_tag t tag =
      match Js.Dict.get t tag with
      | None ->
          let cache = Js.Dict.empty () in
          Js.Dict.set t tag cache;
          cache
      | Some cache -> cache

    let set ~prev_tags (req : _ req) record =
      let by_tag = Lazy.force instance in
      let tags = List.map Tag.to_string req.tags in
      List.iter
        (fun tag ->
          if not (List.mem tag tags) then
            let cache = for_tag by_tag tag in
            By_req.invalidate cache req)
        prev_tags;
      List.iter
        (fun tag ->
          let cache = for_tag by_tag tag in
          let cache, key, _ = By_req.find cache req in
          Js.Dict.set cache key record;
          ())
        tags

    let invalidate tag =
      let t = Lazy.force instance in
      match Js.Dict.get t (Tag.to_string tag) with
      | None -> ()
      | Some cache -> By_req.iter cache ~f:Record.invalidate
  end

  let cached (type a) (req : a req) (f : unit -> string Promise.t) :
      a Promise.t =
    let cache, key, record =
      By_req.find (Lazy.force By_req.instance) req
    in
    let fresh record =
      let json = f () in
      let data, cached_data = cached_data req json in
      (match record with
      | Some record ->
          By_tag.set ~prev_tags:record.Record.tags req record;
          record.json <- Js.Nullable.return json;
          record.data <- Js.Nullable.return cached_data;
          record.tags <- List.map Tag.to_string req.tags
      | None ->
          let record =
            {
              Record.json = Js.Nullable.return json;
              data = Js.Nullable.return cached_data;
              tags = List.map Tag.to_string req.tags;
            }
          in
          By_tag.set ~prev_tags:[] req record;
          Js.Dict.set cache key record);
      data
    in
    match record with
    | None -> fresh None
    | Some record -> (
        match Js.Nullable.toOption record.data with
        | None -> (
            match Js.Nullable.toOption record.json with
            | None -> fresh (Some record)
            | Some json ->
                let data, cached_data = cached_data req json in
                record.data <- Js.Nullable.return cached_data;
                data)
        | Some (Cached_data (key, data)) -> (
            match Witness.equal req.witness key with
            | None ->
                (* This shouldn't happen as requests are identified by
                   path and we already found a corresponding cache record *)
                assert false
            | Some Eq -> data))

  let invalidate req =
    let t = Lazy.force By_req.instance in
    By_req.invalidate t req

  let invalidate_tag = By_tag.invalidate
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

  let to_req route tags =
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
      tags;
    }

  let fetch ?(tags = []) route =
    Cache.cached (to_req route tags) @@ fun () ->
    Promise.(
      let* response = F.fetch' route in
      match Fetch.Response.ok response with
      | true -> Fetch.Response.text response
      | false -> failwith "got non 200")

  let invalidate route = Cache.invalidate (to_req route [])
  let invalidate_tags tags = List.iter Cache.invalidate_tag tags
end
