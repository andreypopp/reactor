open Ppx_deriving_json_runtime.Primitives

type _ t =
  | Home : Ppx_deriving_router_runtime.response t [@GET "/"]
  | About : Ppx_deriving_router_runtime.response t [@GET "/about"]
  | Api_server_time : float t [@GET "/api/server-time"]
[@@deriving router]
