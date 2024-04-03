open Fetch
open Printf
open Promise

(** Borrowed from https://github.com/dbuenzli/hmap/blob/master/src/hmap.ml

    Copyright (c) 2016 Daniel C. Bünzli

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)
module Typed_key : sig
  type ('a, 'b) eq = Eq : ('a, 'a) eq
  type 'a key

  val create : unit -> 'a key
  val equal : 'a 'b. 'a key -> 'b key -> ('a, 'b) eq option
end = struct
  module T = struct
    type _ t = ..
  end

  module type T = sig
    type t
    type _ T.t += T : t T.t
  end

  type 'a key = (module T with type t = 'a)

  let create () (type s) =
    let module M = struct
      type t = s
      type _ T.t += T : t T.t
    end in
    (module M : T with type t = s)

  type ('a, 'b) eq = Eq : ('a, 'a) eq

  let equal : type a b. a key -> b key -> (a, b) eq option =
   fun a b ->
    let module A = (val a : T with type t = a) in
    let module B = (val b : T with type t = b) in
    match A.T with B.T -> Some Eq | _ -> None
end

type json = Js.Json.t

type 'a req = {
  path : string;
  input : string Lazy.t;
  output_of_yojson : json -> 'a;
  key : 'a Typed_key.key;
}

type 'b query = Query : 'b req -> 'b query [@@unboxed]
type 'b mutation = Mutation : 'b req -> 'b mutation [@@unboxed]
type ('a, 'b) query_endpoint = 'a -> 'b query
type ('a, 'b) mutation_endpoint = 'a -> 'b mutation

let define_query ~yojson_of_input ~output_of_yojson ~path =
  let key = Typed_key.create () in
  fun input ->
    Query
      {
        key;
        path;
        input = lazy (Js.Json.stringify (yojson_of_input input));
        output_of_yojson;
      }

let define_mutation ~yojson_of_input ~output_of_yojson ~path =
  let key = Typed_key.create () in
  fun input ->
    Mutation
      {
        key;
        path;
        input = lazy (Js.Json.stringify (yojson_of_input input));
        output_of_yojson;
      }

let make_query endpoint input = endpoint input
let make_mutation = make_query

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
    | Cached_data : 'a Typed_key.key * 'a Promise.t -> cached_data

  let cached_data req json =
    let data =
      let* json = json in
      let json = Js.Json.parseExn json in
      return (req.output_of_yojson json)
    in
    data, Cached_data (req.key, data)

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
            match Typed_key.equal req.key key with
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

let run_query (Query req) =
  Cache.cached req @@ fun () ->
  let path =
    sprintf "%s?input=%s" req.path
      (Js.Global.encodeURIComponent (Lazy.force req.input))
  in
  let* response =
    Fetch.fetchWithRequest
    @@ Request.makeWithInit path
    @@ RequestInit.make ~method_:Get ()
  in
  match Fetch.Response.ok response with
  | true -> Response.text response
  | false -> failwith "got non 200"

let run_mutation (Mutation req) =
  let promise =
    let* response =
      Fetch.fetchWithRequest
      @@ Request.makeWithInit req.path
      @@ RequestInit.make ~method_:Post
           ~body:(BodyInit.make (Lazy.force req.input))
           ()
    in
    Response.text response
  in
  let* data = promise in
  return (req.output_of_yojson (Js.Json.parseExn data))

let invalidate (Query req) = Cache.invalidate req
