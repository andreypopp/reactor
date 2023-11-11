open Persistent

type profile = { name : string; age : int } [@@deriving codec]

type user = {
  id : int;
  created_at : float;
  profile : profile;
  pair : int * int;
}
[@@deriving codec, entity]

type subscription = { user_id : int; name : string }
[@@deriving codec, entity]

let () =
  let db =
    let db = init "./persistent.db" in
    create user db;
    create subscription db;
    db
  in
  let%query q =
    from user;
    where (user.id = 3);
    order_by (desc user.created_at);
    left_join
      (from subscription;
       where (subscription.user_id = 3))
      (user.id = subscription.user_id);
    user.profile.name, user.profile.name = "John"
  in
  Q.iter db q ~f:(fun (name, is_john) ->
      print_endline (Printf.sprintf "name=%s, is_john=%b" name is_john))
