type json = Yojson.Safe.t
type 'a key

val make_key : unit -> 'a key

type 'a req

val make :
  yojson_of_output:('a -> json) ->
  key:'a key ->
  path:string ->
  input:json ->
  (unit -> 'a Lwt.t) ->
  'a req

val run : 'a req -> 'a Lwt.t

module Runner_ctx : sig
  type t

  val create : unit -> t

  type running_req =
    | Running_req : {
        path : string;
        input : json;
        yojson_of_output : 'a -> json;
        promise : 'a Promise.t;
      }
        -> running_req

  val with_ctx : t -> (unit -> 'a) -> 'a * running_req list
end
