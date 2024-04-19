# `ppx_router`

A typed router for Dream.

## Usage

Install (custom opam repo is required as for now):
```
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
opam install ppx_router
```

Put this into your `dune` config:
```
(...
 (preprocess (pps ppx_router))
```

Define your routes:
```ocaml
module Routes = struct
  open Ppx_router_runtime.Types

  type t =
    | Home [@GET "/"]
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
end
```

Now we can generate URLs for these routes:
```ocaml
let () =
  assert (Routes.href Home = "/");
  assert (Routes.href (Hello {name="world"; repeat=1} = "/hello/world?repeat=1")
```

and define a handler for them:
```ocaml
let handle = Routes.handle (function
  | Home -> Dream.html "Home page!"
  | Hello {name; repeat} ->
    let name =
      match repeat with
      | Some repeat ->
        List.init repeat (fun _ -> name) |> String.concat ", "
      | None -> name
    in
    Dream.html (Printf.sprintf "Hello, %s" name))
```

Finally we can use the handler in a Dream app:
```ocaml
let () =
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ handle
```
