open Persistent.Builtins

type profile = { name : string; age : int } [@@deriving codec]

type user = {
  id : int; [@primary_key]
  created_at : float;
  profile : profile;
  pair : int * int;
}
[@@deriving codec, entity]

type subscription = {
  id : int; [@primary_key]
  user_id : int;
  name : string;
}
[@@deriving codec, entity]

let () =
  let db =
    let db = Persistent.init "./persistent.db" in
    Persistent.create user db;
    Persistent.create subscription db;
    db
  in
  Persistent.delete subscription db 2;
  Persistent.insert subscription db { user_id = 1; name = "aaa"; id = 2 };
  let%query sub =
    from subscription;
    where (subscription.user_id = 3);
    { user_id = subscription.user_id }
  in
  let%query q =
    u = from user;
    where (u.id = 3);
    order_by (desc u.created_at);
    left_join sub (u.id = sub.user_id);
    where (u.id = 2);
    left_join
      (sub;
       q = { id = sub.user_id })
      (u.id = q.id);
    where (u.id = 2);
    q = { name = u.profile.name; is_john = u.profile.name = "John" };
    where (q.is_john && q.is_john)
  in
  Persistent.iter_query db q ~f:(fun (name, is_john) ->
      print_endline (Printf.sprintf "name=%s, is_john=%b" name is_john))
