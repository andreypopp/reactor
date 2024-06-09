open Routing.Api

module Hello = Remote.Make (struct
  include Hello

  let href route = Routing.href (Routing.Api_hello route)
end)

module Todo = Remote.Make (struct
  include Todo

  let href route = Routing.href (Routing.Api_todo route)
end)
