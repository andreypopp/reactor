open Persistent.Primitives

module User = struct
  type profile = { name : string; age : int } [@@deriving codec]

  type t = {
    id : int; [@primary_key]
    created_at : float;
    profile : profile;
    pair : int * int;
  }
  [@@deriving codec, table ~name:"user"]
end

module Subscription = struct
  type t = { id : int; [@primary_key] user_id : int; name : string }
  [@@deriving codec, table ~name:"subscription" ~unique:user_id]
end

let () =
  let db =
    let db = Persistent.init "./persistent.db" in
    Persistent.create User.t db;
    Persistent.create Subscription.t db;
    db
  in
  Subscription.delete db 2;
  User.upsert db ~created_at:5.0
    ~profile:{ name = "aaaaaa"; age = 34 }
    ~pair:(1, 2) ();
  let%query sub =
    subscription = from Subscription.t;
    where (subscription.user_id = 3)
  in
  let%query q =
    u = from User.t;
    where (u.id = 3);
    order_by (desc u.created_at);
    left_join sub (u.id = sub.user_id);
    where (u.id = 2);
    left_join
      (sub;
       q = { id = sub.user_id })
      (u.id = q.id);
    where (u.id = 2);
    q = { name = u.profile.name; is_john = u.profile.name = "Jo.'hn" };
    where (q.is_john && q.is_john)
  in
  Persistent.iter_query db q ~f:(fun (name, is_john) ->
      print_endline (Printf.sprintf "name=%s, is_john=%b" name is_john))
