open ContainersLabels
open Lwt.Infix
module Witness = Ppx_deriving_router_runtime_lib.Witness

type json = Json.t

module Cache = struct
  module M = Map.Make (struct
    type t = string * string

    let compare = Ord.(pair string string)
  end)

  type record = Record : 'a Witness.t * 'a Promise.t -> record
  type t = record M.t

  let empty = M.empty
  let find = M.find_opt
  let add = M.add
end

module Runner = struct
  type ctx = {
    mutable cache : Cache.t;
    mutable running : running list;
    mutable all_running : running list;
  }

  and running =
    | Running : {
        path : string;
        input : json;
        json_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running

  let create () = { cache = Cache.empty; running = []; all_running = [] }
  let ctx : ctx Lwt.key = Lwt.new_key ()

  let with_ctx ctx' f =
    let v = Lwt.with_value ctx (Some ctx') f in
    let running = ctx'.running in
    ctx'.running <- [];
    v, running

  let with_ctx_async ctx' f =
    let v = Lwt.with_value ctx (Some ctx') f in
    Lwt.map
      (fun v ->
        let running = ctx'.running in
        ctx'.running <- [];
        v, running)
      v

  let wait ctx =
    Lwt.join
    @@ List.filter_map
         ~f:(fun (Running { promise; _ }) ->
           match Lwt.state promise with
           | Fail _exn -> None
           | Return _v -> None
           | Sleep -> Some (promise >|= Fun.const ()))
         ctx.all_running
end

module Make (S : sig
  type 'a t

  val href : 'a t -> string
  val handle : 'a t -> 'a Promise.t
  val body : 'a t -> json option
  val encode_response : 'a t -> 'a -> json
  val witness : 'a t -> 'a Witness.t
end) =
struct
  type 'a route = 'a S.t

  let handle = S.handle
  let run : type a. a route -> a Lwt.t = handle

  let fetch : type a. a route -> a Lwt.t =
   fun route ->
    let witness = S.witness route in
    let ctx =
      match Lwt.get Runner.ctx with
      | None ->
          failwith
            "no Runner_ctx.t available, did you forgot to wrap the call \
             site with Runner_ctx.with_ctx?"
      | Some ctx -> ctx
    in
    let path = S.href route in
    let input_json = Option.value (S.body route) ~default:`Null in
    let key = path, Yojson.Basic.to_string input_json in
    let fetch () =
      let promise = S.handle route in
      ctx.cache <-
        Cache.add key (Cache.Record (witness, promise)) ctx.cache;
      let running =
        Runner.Running
          {
            path;
            input = input_json;
            json_of_output = S.encode_response route;
            promise;
          }
      in
      ctx.running <- running :: ctx.running;
      ctx.all_running <- running :: ctx.all_running;
      promise
    in
    match Cache.find key ctx.cache with
    | Some (Record (witness', promise)) -> (
        match Witness.equal witness' witness with
        | Some Eq -> promise
        | None -> assert false)
    | None -> fetch ()
end
