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
