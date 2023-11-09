[@@@ocaml.warning "-27-32-33-34-37-69"]

open Printf
open ContainersLabels
open Persistent

type profile = { name : string; age : int } [@@deriving codec]

type user = {
  id : int;
  created_at : float;
  profile : profile;
  profile_old : profile;
  pair : int * int;
}
[@@deriving codec, entity]

type subscription = { user_id : int; name : string }
[@@deriving codec, entity]

(* let q q = *)
(*   Q.from user *)
(*   |> [%select *)
(*        user, *)
(*          { *)
(*            name = user.profile.name; *)
(*            is_john = user.profile.name = "John"; *)
(*          }] *)

let () =
  let open Q in
  let db = Sqlite3.db_open "./persistent.db" in
  Persistent.init user db;
  Persistent.init subscription db;
  let q =
    from user
    |> where E.(fun user -> user#id = int 3)
    |> order_by E.(fun user -> [ desc user#created_at ])
    |> left_join (from subscription)
         E.(fun (user, sub) -> user#id = sub#user_id)
  in
  (* q |> iter db ~f:(fun (u, _) -> print_endline u.profile.name) *)
  P.iter q
    E.(
      fun (user, _) ->
        P.(
          let+ name = get user#profile#name
          and+ is_john = get (user#profile#name = string "John") in
          name, is_john))
    db
    ~f:(fun (name, is_john) ->
      print_endline (Printf.sprintf "name=%s, is_john=%b" name is_john))

(*
let%query q =
  Q.from user
  |> Q.where (here.user.id = 3)
  |> Q.order_by (desc here.user.created_at)
  |> Q.left_join (Q.from subscription) ~on:(here.id = subscription.user_id)
  |> Q.select
       {
         name = user.profile.name;
         is_john = user.profile.name = "John";
         sub_name = sub.name;
       }
  |> Q.group_by here.sub_name
  |> Q.select { sub_name = here.sub_name; count = group.count () }
 *)

(*
   TODO: make a ppx, so the above query can be written as:

   FROM user
   WHERE user.id = 3
   ORDER BY user.created_at
   LEFT JOIN
     (FROM subscription) AS sub
     ON user.id = sub.user_id
   SELECT
     user.profile.name AS name,
     user.profile.name = 'John' AS is_john,
     sub.name AS sub_name
   GROUP BY sub_name
   SELECT sub_name, group.count()
*)
