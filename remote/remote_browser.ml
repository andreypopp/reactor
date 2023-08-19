open Fetch
open Promise

type json = Yojson.Safe.t

type 'a req = {
  path : string;
  params_str : string Lazy.t;
  output_of_yojson : json -> 'a;
}

let make ~output_of_yojson path params =
  {
    path;
    params_str = lazy (Yojson.Safe.to_string params);
    output_of_yojson;
  }

module Cache : sig
  val find : 'a req -> string Promise.t option
  val set : 'a req -> string Promise.t -> unit
  (* val prune : 'a req -> unit *)
end = struct
  type t = string Promise.t Js.Dict.t Js.Dict.t

  external t : t = "window.__remote_cache"

  let find req : string Promise.t option =
    match Js.Dict.get t req.path with
    | None -> None
    | Some t' -> Js.Dict.get t' (Lazy.force req.params_str)

  let set req json =
    let t' =
      match Js.Dict.get t req.path with
      | None ->
          let t' = Js.Dict.empty () in
          Js.Dict.set t req.path t';
          t'
      | Some t' -> t'
    in
    Js.Dict.set t' (Lazy.force req.params_str) json

  (* let prune req = *)
  (*   match Js.Dict.get t req.path with *)
  (*   | None -> () *)
  (*   | Some t -> *)
  (*       Js.Dict.unsafeDeleteKey (Obj.magic t) *)
  (*         (Lazy.force req.params_str) [@bs] *)
end

let run req =
  match Cache.find req with
  | Some promise ->
      let* data = promise in
      let json = Yojson.Safe.from_string data in
      return (req.output_of_yojson json)
  | None ->
      let promise =
        let* resp =
          Fetch.fetchWithRequest
          @@ Request.makeWithInit req.path
               (RequestInit.make ~method_:Post
                  ~body:(BodyInit.make (Lazy.force req.params_str))
                  ())
        in
        let* data = Response.text resp in
        return data
      in
      Cache.set req promise;
      let* data = promise in
      return (req.output_of_yojson (Yojson.Safe.from_string data))
