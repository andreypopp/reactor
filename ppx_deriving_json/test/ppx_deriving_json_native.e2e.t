
  $ echo '(lang dune 3.11)' > dune-project
  $ echo '
  > (executable 
  >   (name main)
  >   (libraries yojson)
  >   (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  >   (preprocess (pps ppx_deriving_json.native)))' > dune

  $ echo '
  > open Example
  > let () = Cases.run ()
  >   ~json_to_string:Yojson.Basic.to_string
  >   ~json_of_string:Yojson.Basic.from_string
  > ' >> main.ml

  $ dune build ./main.exe

  $ dune exec ./main.exe
  JSON    DATA: 1
  JSON REPRINT: 1
  JSON    DATA: "OK"
  JSON REPRINT: "OK"
  JSON    DATA: "some"
  JSON REPRINT: "some"
  JSON    DATA: [42, "works"]
  JSON REPRINT: [42,"works"]
  JSON    DATA: {"name":"N","age":1}
  JSON REPRINT: {"name":"N","age":1}
  JSON    DATA: ["A"]
  JSON REPRINT: ["A"]
  JSON    DATA: ["B", 42]
  JSON REPRINT: ["B",42]
  JSON    DATA: ["C", {"name": "cname"}]
  JSON REPRINT: ["C",{"name":"cname"}]
  JSON    DATA: ["A"]
  JSON REPRINT: ["A"]
  JSON    DATA: ["B", 42]
  JSON REPRINT: ["B",42]
  JSON    DATA: ["Fix",["Fix",["Fix",["A"]]]]
  JSON REPRINT: ["Fix",["Fix",["Fix",["A"]]]]
  JSON    DATA: ["Fix",["Fix",["Fix",["A"]]]]
  JSON REPRINT: ["Fix",["Fix",["Fix",["A"]]]]
