
  $ echo '(lang dune 3.11) 
  > (using melange 0.1)' > dune-project

  $ echo '
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules example main)
  >  (flags :standard -w -37-69 -open Ppx_deriving_json_runtime.Primitives)
  >  (preprocess (pps melange.ppx ppx_deriving_json.browser)))
  > (melange.emit
  >  (alias js)
  >  (target output)
  >  (modules)
  >  (libraries lib)
  >  (module_systems commonjs))' > dune

  $ echo '
  > let print_json v to_json =
  >   let json = to_json v in
  >   print_endline (Js.Json.stringify json)
  > let () = 
  >   let open Example.To_json in
  >   print_json 1 user_to_json;
  >   print_json ("OK" : string param) (param_to_json string_to_json);
  >   print_json (None : opt) opt_to_json;
  >   print_json (Some "some" : opt) opt_to_json;
  >   print_json (42, "works") tuple_to_json;
  >   print_json (A : sum) sum_to_json;
  >   print_json (B 42 : sum) sum_to_json;
  >   print_json (C {name="cname"} : sum) sum_to_json;
  >   print_json (`A : poly) poly_to_json;
  >   print_json (`B 42 : poly) poly_to_json;
  >   print_json (`C : poly) poly_to_json;
  >   print_json (Fix (Fix (Fix A)) : recur) recur_to_json;
  >   print_json (`Fix (`Fix (`Fix `A)) : polyrecur) polyrecur_to_json;
  >   print_endline "PASS"
  > let of_json (type a) data (of_json : Js.Json.t -> a) (e : a) =
  >   let json = Js.Json.parseExn data in
  >   let v = of_json json in
  >   assert (v = e);
  >   print_endline data
  > let () =
  >   let open Example.Of_json in
  >   of_json "1" user_of_json 1;
  >   of_json {|"OK"|} (param_of_json string_of_json) "OK";
  >   of_json {|"some"|} opt_of_json (Some "some");
  >   of_json {|[42, "works"]|} tuple_of_json (42, "works");
  >   of_json {|["A"]|} sum_of_json (A : sum);
  >   of_json {|["B", 42]|} sum_of_json (B 42 : sum);
  >   of_json {|["C", {"name": "cname"}]|} sum_of_json (C {name="cname"} : sum);
  >   of_json {|["A"]|} poly_of_json (`A : poly);
  >   of_json {|["B", 42]|} poly_of_json (`B 42 : poly);
  >   of_json {|["Fix",["Fix",["Fix",["A"]]]]|} recur_of_json (Fix (Fix (Fix A)));
  >   of_json {|["Fix",["Fix",["Fix",["A"]]]]|} polyrecur_of_json (`Fix (`Fix (`Fix `A)));
  >   print_endline "PASS"
  > ' >> main.ml

  $ dune build @js

  $ node ./_build/default/output/main.js
  1
  "OK"
  null
  "some"
  [42,"works"]
  ["A"]
  ["B",42]
  ["C",{"name":"cname"}]
  ["A"]
  ["B",42]
  ["C"]
  ["Fix",["Fix",["Fix",["A"]]]]
  ["Fix",["Fix",["Fix",["A"]]]]
  PASS
  1
  "OK"
  "some"
  [42, "works"]
  ["A"]
  ["B", 42]
  ["C", {"name": "cname"}]
  ["A"]
  ["B", 42]
  ["Fix",["Fix",["Fix",["A"]]]]
  ["Fix",["Fix",["Fix",["A"]]]]
  PASS