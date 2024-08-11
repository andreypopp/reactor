open ContainersLabels
open Lwt.Infix
module Witness = Ppx_deriving_router_runtime_lib.Witness

type json = Json.t

module Tag : sig
  type t

  val make : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let make x = x
  let to_string x = x
end

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

module Context = struct
  type t = {
    mutable cache : Cache.t;
    mutable running : fetch list;
    mutable all_running : fetch list;
    mutable runtime_emitted : bool;
  }

  and fetch =
    | Fetch : {
        path : string;
        input : json;
        json_of_output : 'a -> json;
        promise : 'a Promise.t;
        tags : Tag.t list;
      }
        -> fetch

  and batch = fetch list

  let create () =
    {
      cache = Cache.empty;
      running = [];
      all_running = [];
      runtime_emitted = false;
    }

  let current : t Lwt.key = Lwt.new_key ()

  let with_ctx ctx' f =
    let v = Lwt.with_value current (Some ctx') f in
    let running = ctx'.running in
    ctx'.running <- [];
    v, running

  let with_ctx_async ctx' f =
    let v = Lwt.with_value current (Some ctx') f in
    Lwt.map
      (fun v ->
        let running = ctx'.running in
        ctx'.running <- [];
        v, running)
      v

  let wait ctx =
    Lwt.join
    @@ List.filter_map
         ~f:(fun (Fetch { promise; _ }) ->
           match Lwt.state promise with
           | Fail _exn -> None
           | Return _v -> None
           | Sleep -> Some (promise >|= Fun.const ()))
         ctx.all_running

  module To_html = struct
    let runtime =
      Htmlgen.unsafe_raw
        {|
        <script>
        window.__Remote = window.__Remote || {};
        window.__Remote.push = () => {
          let el = document.currentScript;
          let path = el.dataset.path;
          let input = el.dataset.input;
          let tags = JSON.parse(el.dataset.tags);
          let output = el.dataset.output;
          let record = {json: Promise.resolve(output), data: null, tags};

          let by_req = window.__Remote_cache = window.__Remote_cache || {};
          let by_tag = window.__Remote_cache_by_tag = window.__Remote_cache_by_tag || {};

          by_req[path]        = by_req[path] || {};
          by_req[path][input] = record;

          tags.forEach(tag => {
            by_tag[tag]              = by_tag[tag] || {};
            by_tag[tag][path]        = by_tag[tag][path] || {};
            by_tag[tag][path][input] = record;
          });
        };
        </script>
    |}

    let fetch_to_html' ~path ~input ~tags output =
      let tags =
        `List (List.map tags ~f:(fun tag -> `String (Tag.to_string tag)))
      in
      Htmlgen.unsafe_rawf
        "<script data-path='%s' data-input='%s' data-tags='%s' \
         data-output='%s'>window.__Remote.push()</script>"
        (Htmlgen.single_quote_escape path)
        (Htmlgen.single_quote_escape (Yojson.Basic.to_string input))
        (Htmlgen.single_quote_escape (Yojson.Basic.to_string tags))
        (Htmlgen.single_quote_escape (Yojson.Basic.to_string output))

    let fetch_to_html
        (Fetch { path; input; promise; json_of_output; tags }) =
      promise >|= fun output ->
      fetch_to_html' ~tags ~path ~input (json_of_output output)

    let batch_to_html ctx = function
      | [] -> Lwt.return Htmlgen.empty
      | batch ->
          Lwt_list.map_p fetch_to_html batch >|= fun htmls ->
          let htmls =
            if not ctx.runtime_emitted then (
              ctx.runtime_emitted <- true;
              runtime :: htmls)
            else htmls
          in
          Htmlgen.splice htmls
  end

  let batch_to_html = To_html.batch_to_html
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

  let fetch : type a. ?tags:Tag.t list -> a route -> a Lwt.t =
   fun ?(tags = []) route ->
    let witness = S.witness route in
    let ctx =
      match Lwt.get Context.current with
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
        Context.Fetch
          {
            path;
            input = input_json;
            json_of_output = S.encode_response route;
            promise;
            tags;
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
