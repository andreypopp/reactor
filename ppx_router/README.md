# `ppx_router`

A typed router for Dream.

## Usage

Install (custom opam repo is required as for now):
```
opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
opam update
opam install ppx_router
```

Put this into your `dune` file:
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
let handle = Routes.handle (fun route _req ->
  match route with
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
let () = Dream.run ~interface:"0.0.0.0" ~port:8080 handle
```

## Custom path/query parameter types

When generating parameter encoding/decoding code for a parameter of type `T`,
`ppx_router` will emit the code that uses the following functions.

If `T` is a path parameter:
```ocaml
val T_of_url_path : string -> T option
val T_to_url_path : T -> string
```

If `T` is a query parameter:
```ocaml
val T_of_url_query : string list -> T option
val T_to_url_query : T -> string list
```

The default encoders/decoders are provided in `Ppx_router_runtime.Types` module
(this is why we need to `open` the module when defining routes).

To provide custom encoders/decoders for a custom type, we can define own
functions, for example:

```ocaml
module Modifier = struct
  type t = Capitalize | Uppercase

  let rec of_url_query : t Ppx_router_runtime.url_query_decoder = function
    | [] -> None
    | [ "capitalize" ] -> Some Capitalize
    | [ "uppercase" ] -> Some Uppercase
    | _ :: xs -> of_url_query xs (* let the last one win *)

  let to_url_query : t Ppx_router_runtime.url_query_encoder = function
    | Capitalize -> [ "capitalize" ]
    | Uppercase -> [ "uppercase" ]
end
```

After that one can use `Modifier.t` in route definitions:

```ocaml
type t =
  | Hello of { name : string; modifier : Modifier.t } [@GET "/hello/:name"]
  [@@deriving router]
```
