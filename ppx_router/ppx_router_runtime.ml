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
