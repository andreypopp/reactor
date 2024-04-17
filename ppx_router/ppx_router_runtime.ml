type 'handle route = {
  method_ : [ `GET | `POST | `PUT | `DELETE ];
  path : string;
  handle : 'handle;
}

let to_route (route : _ route) f : Dream.route =
  let k h = h in
  let register =
    match route.method_ with
    | `GET -> Dream.get
    | `POST -> Dream.post
    | `PUT -> Dream.put
    | `DELETE -> Dream.delete
  in
  register route.path (route.handle k f)

let encode_path out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Path x)

let encode_query_key out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_key x)

let encode_query_value out x =
  Buffer.add_string out (Uri.pct_encode ~component:`Query_value x)

module Types = struct
  let string_to_url x = x
  let string_of_url x = Some x
  let int_to_url x = string_of_int x
  let int_of_url x = int_of_string_opt x
  let bool_to_url x = if x then "true" else "false"

  let bool_of_url x =
    match x with "true" -> Some true | "false" -> Some false | _ -> None
end
