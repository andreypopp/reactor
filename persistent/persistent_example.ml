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

let user_scope id t =
  object
    method id = E.col id t "id" int_decode
    method created_at = E.col id t "created_at" float_decode

    method pair =
      E.col id t "pair_0" int_decode, E.col id t "pair_1" int_decode

    method profile =
      object
        method name = E.col id t "profile_name" string_decode
        method age = E.col id t "profile_age" int_decode
      end
  end

let user_fields =
  [
    Any_expr (E.col 0 "t" "id" int_decode), "id";
    Any_expr (E.col 0 "t" "created_at" float_decode), "created_at";
    Any_expr (E.col 0 "t" "pair_0" int_decode), "pair_0";
    Any_expr (E.col 0 "t" "pair_1" int_decode), "pair_1";
    Any_expr (E.col 0 "t" "profile_name" string_decode), "profile_name";
    Any_expr (E.col 0 "t" "profile_age" int_decode), "profile_age";
  ]

let subscription_scope id t =
  object
    method user_id = E.col id t "user_id" int_decode
    method name = E.col id t "name" string_decode
  end

let subscription_fields =
  [
    Any_expr (E.col 0 "t" "user_id" int_decode), "user_id";
    Any_expr (E.col 0 "t" "name" float_decode), "name";
  ]

let () =
  let open Q in
  let db = Sqlite3.db_open "./persistent.db" in
  Persistent.init user db;
  Persistent.init subscription db;
  let q =
    from user user_fields user_scope
    |> where E.(fun user -> user#id = int 3)
    |> order_by E.(fun user -> [ desc user#created_at ])
    |> left_join
         (from subscription subscription_fields subscription_scope)
         E.(fun (user, sub) -> user#id = sub#user_id)
    (*
       FROM user
       WHERE user.id = 3
       ORDER BY user.created_at
       LEFT JOIN (FROM subscription) AS sub ON user.id = sub.user_id
    *)
  in
  (* q |> iter db ~f:(fun (u, _) -> print_endline u.profile.name) *)
  q
  |> P.(
       iter
         E.(
           fun (user, _) ->
             let+ name = get user#profile#name
             and+ is_john = get (user#profile#name = string "John") in
             name, is_john)
         db
         ~f:(fun (name, is_john) ->
           print_endline
             (Printf.sprintf "name=%s, is_john=%b" name is_john)))

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
