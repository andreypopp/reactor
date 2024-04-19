type 'a url_path_encoder = 'a -> string
type 'a url_path_decoder = string -> 'a option
type 'a url_query_encoder = 'a -> string list
type 'a url_query_decoder = string list -> 'a option

module Types = struct
  let string_to_url_path x = x
  let string_of_url_path x = Some x
  let int_to_url_path x = string_of_int x
  let int_of_url_path x = int_of_string_opt x
  let bool_to_url_path x = if x then "true" else "false"

  let bool_of_url_path x =
    match x with "true" -> Some true | "false" -> Some false | _ -> None

  let rec last_wins f = function
    | [] -> None
    | [ x ] -> f x
    | _ :: xs -> last_wins f xs

  let string_to_url_query x = [ x ]
  let string_of_url_query = last_wins (fun x -> Some x)
  let int_to_url_query x = [ string_of_int x ]
  let int_of_url_query = last_wins int_of_string_opt
  let bool_to_url_query x = if x then [ "true" ] else []

  let bool_of_url_query =
    last_wins (function "true" -> Some true | _ -> Some false)

  let option_to_url_query :
      'a url_query_encoder -> 'a option url_query_encoder =
   fun f x -> match x with None -> [] | Some v -> f v

  let option_of_url_query :
      'a url_query_decoder -> 'a option url_query_decoder =
   fun f x ->
    match x with
    | [] -> Some None
    | x -> ( match f x with None -> None | Some v -> Some (Some v))
end

let encode_path out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Path x)

let encode_query_key out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_key x)

let encode_query_value out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_value x)

exception Method_not_allowed
exception Invalid_query_parameter of string * string list

type 'a router = (Dream.request -> 'a) Routes.router

let make x = x

let route (router : _ router) req =
  let target = Dream.target req in
  match Routes.match' router ~target with
  | Routes.FullMatch v | Routes.MatchWithTrailingSlash v -> (
      match v req with
      | v -> `Ok v
      | exception Invalid_query_parameter (x, y) ->
          `Invalid_query_parameter (x, y)
      | exception Method_not_allowed -> `Method_not_allowed)
  | Routes.NoMatch -> `Not_found

let handle (router : _ router) f req =
  match route router req with
  | `Ok v -> f v req
  | `Invalid_query_parameter (param, _) ->
      Dream.respond ~status:`Bad_Request
        (Printf.sprintf "Invalid or missing query parameter: %s" param)
  | `Method_not_allowed ->
      Dream.respond ~status:`Method_Not_Allowed "Method not allowed"
  | `Not_found -> Dream.respond ~status:`Not_Found "Not found"
