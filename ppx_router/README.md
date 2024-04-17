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

Now define your routes:
```ocaml
open Ppx_router_runtime.Types

let main, main_href =
  [%GET "/"]

let hello, hello_href =
  [%GET "/hello/:name?repeat=:int"]
```

And finally use them in your Dream app:
```ocaml
let () =
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ Dream.router [
    Ppx_router_runtime.to_route main (fun _req ->
      Dream.html "Hello, World!");
    Ppx_router_runtime.to_route hello (fun ~name ~repeat _req ->
      let name =
        match repeat with
        | Some repeat ->
          List.init repeat (fun _ -> name) |> String.concat ", "
        | None -> name
      in
      Dream.html (Printf.sprintf "Hello, %s" name));
  ]
```

The functions `main_href` and `hello_href` are used to generate URLs for the routes.
