open ContainersLabels

type json = Yojson.Safe.t

type 'output query =
  | Query : {
      f : 'input -> 'output Lwt.t;
      path : string;
      input : 'input;
      yojson_of_output : 'output -> json;
      yojson_of_input : 'input -> json;
      query_key : 'output query_key;
    }
      -> 'output query

and 'a query_key = (string * string, 'a Promise.t) Hashtbl.t Hmap.key
and ('input, 'output) query_def = 'input -> 'output query

let define_query ~yojson_of_input ~yojson_of_output ~path
    (f : 'a -> 'b Lwt.t) : ('a, 'b) query_def =
  let query_key = Hmap.Key.create () in
  fun input ->
    Query { f; yojson_of_output; yojson_of_input; path; input; query_key }

let make_query def input = def input

module Runner = struct
  type ctx = { mutable cache : Hmap.t; mutable running : running list }

  and running =
    | Running : {
        path : string;
        input : json;
        yojson_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running

  let create () = { cache = Hmap.empty; running = [] }
  let ctx : ctx option ref = ref None

  let with_ctx ctx' f =
    ctx := Some ctx';
    let v = Fun.protect f ~finally:(fun () -> ctx := None) in
    let running = ctx'.running in
    ctx'.running <- [];
    v, running
end

let run
    (Query
      { f; path; input; yojson_of_input; yojson_of_output; query_key }) =
  match !Runner.ctx with
  | None ->
      failwith
        "no Runner_ctx.t available, did you forgot to wrap the call site \
         with Runner_ctx.with_ctx?"
  | Some ctx -> (
      let cache =
        match Hmap.find query_key ctx.cache with
        | Some cache -> cache
        | None ->
            let cache = Hashtbl.create 10 in
            ctx.cache <- Hmap.add query_key cache ctx.cache;
            cache
      in
      let input_json = yojson_of_input input in
      let key = path, Yojson.Safe.to_string input_json in
      match Hashtbl.find_opt cache key with
      | Some promise -> promise
      | None ->
          let promise = f input in
          Hashtbl.replace cache key promise;
          ctx.running <-
            Running
              { path; input = input_json; yojson_of_output; promise }
            :: ctx.running;
          promise)
