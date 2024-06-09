type _ t =
  | Home : Ppx_deriving_router_runtime.response t [@GET "/"]
  | About : Ppx_deriving_router_runtime.response t [@GET "/about"]
[@@deriving router]
