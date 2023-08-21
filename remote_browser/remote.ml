open Fetch
open Printf
open Promise

type json = Yojson.Safe.t

type 'a req = { query_key : query_key; output_of_yojson : json -> 'a }
and query_key = { path : string; input : string Lazy.t }

type 'b query = Query : 'b req -> 'b query
type 'b mutation = Mutation : 'b req -> 'b mutation
type ('a, 'b) query_endpoint = 'a -> 'b query
type ('a, 'b) mutation_endpoint = 'a -> 'b mutation

let define_query ~yojson_of_input ~output_of_yojson ~path input =
  Query
    {
      query_key =
        {
          path;
          input = lazy (Yojson.Safe.to_string (yojson_of_input input));
        };
      output_of_yojson;
    }

let define_mutation ~yojson_of_input ~output_of_yojson ~path input =
  Mutation
    {
      query_key =
        {
          path;
          input = lazy (Yojson.Safe.to_string (yojson_of_input input));
        };
      output_of_yojson;
    }

let make_query endpoint input = endpoint input
let make_mutation = make_query

module Cache : sig
  val find : query_key -> string Promise.t option
  val set : query_key -> string Promise.t -> unit
  val invalidate : query_key -> unit
end = struct
  type t = string Promise.t Js.Dict.t Js.Dict.t

  external t : t = "window.__Remote_cache"

  let find req : string Promise.t option =
    match Js.Dict.get t req.path with
    | None -> None
    | Some t' -> Js.Dict.get t' (Lazy.force req.input)

  let set req json =
    let t' =
      match Js.Dict.get t req.path with
      | None ->
          let t' = Js.Dict.empty () in
          Js.Dict.set t req.path t';
          t'
      | Some t' -> t'
    in
    Js.Dict.set t' (Lazy.force req.input) json

  let invalidate req =
    match Js.Dict.get t req.path with
    | None -> ()
    | Some t ->
        Js.Dict.unsafeDeleteKey (Obj.magic t) (Lazy.force req.input) [@bs]
end

let run_query (Query req) =
  match Cache.find req.query_key with
  | Some promise ->
      let* data = promise in
      let json = Yojson.Safe.from_string data in
      return (req.output_of_yojson json)
  | None ->
      let promise =
        let path =
          sprintf "%s?input=%s" req.query_key.path
            (Js.Global.encodeURIComponent
               (Lazy.force req.query_key.input))
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
      Cache.set req.query_key promise;
      let* data = promise in
      return (req.output_of_yojson (Yojson.Safe.from_string data))

let invalidate (Query query) = Cache.invalidate query.query_key

let run_mutation (Mutation req) =
  let promise =
    let* response =
      Fetch.fetchWithRequest
      @@ Request.makeWithInit req.query_key.path
      @@ RequestInit.make ~method_:Post
           ~body:(BodyInit.make (Lazy.force req.query_key.input))
           ()
    in
    Response.text response
  in
  let* data = promise in
  return (req.output_of_yojson (Yojson.Safe.from_string data))
