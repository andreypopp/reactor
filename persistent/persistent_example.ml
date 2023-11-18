open Persistent.Primitives

module User = struct
  type profile = { name : string; age : int } [@@deriving codec, meta]

  type t = {
    id : int;
    created_at : float;
    profile : profile;
    pair : int * int;
  }
  [@@deriving codec, meta, table ~name:"user"]
  [@@persistent.primary_key id]
end

module Subscription = struct
  type t = { id : int; user_id : int; name : string }
  [@@deriving codec, meta, table ~name:"subscription"]
  [@@persistent.primary_key id]
  [@@persistent.unique id, user_id]
end

module Submodule = struct
  let%query sub () =
    from Subscription.t;
    where (t.user_id = 3)
end

let%expr is_john u = u.profile.name = "John" && true
let where_user_id id = fun%query u -> where (u.id = Persistent.E.int id)

(** select a single value *)
let () =
  let%query q =
    from Subscription.t;
    select t.user_id;
    select (t = 1)
  in
  Persistent.Q.print_sql q

(** select a tuple value *)
let () =
  let%query q =
    from Subscription.t;
    select (t.user_id, "OK");
    (u, n) = where (t._1 = 45);
    where (u = 45)
  in
  Persistent.Q.print_sql q

(** select a record value *)
let () =
  let%query q =
    from Subscription.t;
    { user_id } = where (t.user_id = 45);
    select { is_45 = user_id = 45 }
  in
  Persistent.Q.print_sql q

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
    from User.t;
    query (where_user_id 3);
    u = order_by (desc t.created_at);
    (user, sub) = left_join (query Submodule.sub) (u.id = t.user_id);
    where (user.id = 2 && sub#.id = 1);
    left_join
      (query Submodule.sub;
       t' = { id = t.user_id })
      (user.id = t'.id);
    where (user.id = 2);
    q
    = {
        name = user.profile.name;
        is_john = is_john user;
        x = (if is_john user then 1 else 0);
        zz = nullable sub#.user_id;
      };
    where (q.is_john && q.is_john)
  in
  Persistent.iter_query db q ~f:(fun (name, is_john, x, _zz) ->
      print_endline
        (Printf.sprintf "name=%s, is_john=%b, x=%i" name is_john x))
