open Ppx_deriving_json_runtime.Primitives
open Ppx_deriving_router_runtime.Types

module Api = struct
  module Hello = struct
    type greeting = Greeting_formal | Greeting_informal
    [@@deriving json]

    type _ t =
      | Hello : { name : string } -> string t [@GET "/"]
      | Update_greeting : { greeting : greeting [@body] } -> unit t
          [@POST "/"]
    [@@deriving router]
  end

  module Todo = struct
    type todo = { id : int; text : string; completed : bool }
    [@@deriving json]

    type _ t =
      | List : todo list t [@GET "/"]
      | Create : { text : string } -> todo t [@POST "/"]
      | Remove_completed : unit t [@DELETE "/completed"]
      | Update : {
          id : int;
          text : string option;
          completed : bool option; [@body]
        }
          -> todo option t [@PUT "/:id"]
    [@@deriving router]
  end
end

type _ t =
  | Home : Ppx_deriving_router_runtime.response t [@GET "/"]
  | About : Ppx_deriving_router_runtime.response t [@GET "/about"]
  | Todo : Ppx_deriving_router_runtime.response t [@GET "/todo"]
  | No_ssr : Ppx_deriving_router_runtime.response t [@GET "/no-ssr"]
  | Api_todo : 'a Api.Todo.t -> 'a t [@prefix "/api/todo"]
  | Api_hello : 'a Api.Hello.t -> 'a t [@prefix "/api/hello"]
[@@deriving router]
