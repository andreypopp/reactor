type t =
  | Home [@GET "/"]
  | About [@GET "/about"]
  | Todo [@GET "/todo"]
  | No_ssr [@GET "/no-ssr"]
[@@deriving router]
