open ContainersLabels

type json = Yojson.Safe.t

type 'a req = {
  run : unit -> 'a Lwt.t;
  path : string;
  input : json;
  yojson_of_output : 'a -> json;
  key : 'a key;
}

and 'a key = (string * string, 'a Promise.t) Hashtbl.t Hmap.key

let make_key () = Hmap.Key.create ()

let make ~yojson_of_output ~key ~path ~input run =
  { run; yojson_of_output; path; input; key }

module Runner_ctx = struct
  type t = {
    mutable cache : Hmap.t;
    mutable running_reqs : running_req list;
  }

  and running_req =
    | Running_req : {
        path : string;
        input : json;
        yojson_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running_req

  let create () = { cache = Hmap.empty; running_reqs = [] }
  let ctx : t option ref = ref None

  let with_ctx ctx' f =
    ctx := Some ctx';
    let v = Fun.protect f ~finally:(fun () -> ctx := None) in
    let running_reqs = ctx'.running_reqs in
    ctx'.running_reqs <- [];
    v, running_reqs
end

let run { run; path; input; yojson_of_output; key } =
  match !Runner_ctx.ctx with
  | None ->
      failwith
        "no Runner_ctx.t available, did you forgot to wrap the call site \
         with Runner_ctx.with_ctx?"
  | Some ctx -> (
      let cache =
        match Hmap.find key ctx.cache with
        | Some cache -> cache
        | None ->
            let cache = Hashtbl.create 10 in
            ctx.cache <- Hmap.add key cache ctx.cache;
            cache
      in
      let key = path, Yojson.Safe.to_string input in
      match Hashtbl.find_opt cache key with
      | Some promise -> promise
      | None ->
          let promise = run () in
          Hashtbl.replace cache key promise;
          ctx.running_reqs <-
            Running_req { path; input; yojson_of_output; promise }
            :: ctx.running_reqs;
          promise)
