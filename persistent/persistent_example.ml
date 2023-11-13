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

module Submodule = struct
  let%query sub =
    from Subscription.t;
    where (t.user_id = 3)
end

let%expr is_john u = u.profile.name = "John" && true

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
  let%query q =
    u = from User.t;
    where (u.id = 3);
    order_by (desc u.created_at);
    left_join Submodule.sub (u.id = sub.user_id);
    where (u.id = 2);
    left_join
      (Submodule.sub;
       q = { id = sub.user_id })
      (u.id = q.id);
    where (u.id = 2);
    q
    = {
        name = u.profile.name;
        is_john = is_john u;
        x = (if is_john u then 1 else 0);
      };
    where (q.is_john && q.is_john)
  in
  Persistent.iter_query db q ~f:(fun (name, is_john, x) ->
      print_endline (Printf.sprintf "name=%s, is_john=%b, x=%i" name is_john x))
