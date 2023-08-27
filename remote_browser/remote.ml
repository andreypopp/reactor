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

type json = Yojson.Safe.t

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
        input = lazy (Yojson.Safe.to_string (yojson_of_input input));
        output_of_yojson;
      }

let define_mutation ~yojson_of_input ~output_of_yojson ~path =
  let key = Typed_key.create () in
  fun input ->
    Mutation
      {
        key;
        path;
        input = lazy (Yojson.Safe.to_string (yojson_of_input input));
        output_of_yojson;
      }

let make_query endpoint input = endpoint input
let make_mutation = make_query

module Cache : sig
  val find : 'a query -> 'a Promise.t option
  val set : 'a query -> string Promise.t -> 'a Promise.t
  val invalidate : 'a query -> unit
end = struct
  type cached_data =
    | Cached_data : 'a Typed_key.key * 'a Promise.t -> cached_data

  let cache_data req json =
    let data =
      let* json = json in
      let json = Yojson.Safe.from_string json in
      return (req.output_of_yojson json)
    in
    data, Cached_data (req.key, data)

  type record = {
    json : string Promise.t;
    mutable data : cached_data Js.nullable;
  }

  type t = record Js.Dict.t Js.Dict.t

  external window : t Js.Dict.t = "window"
  external t : t Js.Undefined.t = "window.__Remote_cache"

  let ( let@ ) = Option.bind

  let find (type a) (Query req : a query) : a Promise.t option =
    let@ t = Js.Undefined.toOption t in
    let@ t' = Js.Dict.get t req.path in
    let@ record = Js.Dict.get t' (Lazy.force req.input) in
    match Js.Nullable.toOption record.data with
    | None ->
        let data, cached_data = cache_data req record.json in
        record.data <- Js.Nullable.return cached_data;
        Some data
    | Some (Cached_data (key, data)) -> (
        match Typed_key.equal req.key key with
        | None -> None
        | Some Eq -> Some data)

  let set (Query req : 'a query) json =
    let t =
      match Js.Undefined.toOption t with
      | None ->
          let t = Js.Dict.empty () in
          Js.Dict.set window "__Remote_cache" t;
          t
      | Some t -> t
    in
    let t' =
      match Js.Dict.get t req.path with
      | None ->
          let t' = Js.Dict.empty () in
          Js.Dict.set t req.path t';
          t'
      | Some t' -> t'
    in
    let data, cached_data = cache_data req json in
    Js.Dict.set t' (Lazy.force req.input)
      { json; data = Js.Nullable.return cached_data };
    data

  let invalidate (Query req) =
    match Js.Undefined.toOption t with
    | None -> ()
    | Some t -> (
        match Js.Dict.get t req.path with
        | None -> ()
        | Some t ->
            Js.Dict.unsafeDeleteKey (Obj.magic t) (Lazy.force req.input)
            [@bs])
end

let run_query (Query req as q) =
  (* NOTE: it is important not to create fresh promises here *)
  match Cache.find q with
  | Some promise -> promise
  | None ->
      let json =
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
      in
      Cache.set q json

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
  return (req.output_of_yojson (Yojson.Safe.from_string data))

let invalidate q = Cache.invalidate q
