type user = int [@@deriving json]
type 'a param = 'a [@@deriving json]
type opt = string option [@@deriving json]
type tuple = int * string [@@deriving json]
type record = { name : string; age : int } [@@deriving json]
type sum = A | B of int | C of { name : string } [@@deriving json]
type other = [ `C ] [@@deriving json] type poly = [ `A | `B of int | other ] [@@deriving json]
type 'a c = [ `C of 'a ] [@@deriving json]
type recur = A | Fix of recur [@@deriving json]
type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]

module Cases = struct 
  type json = Ppx_deriving_json_runtime.t
  type of_json = C : string * (json -> 'a) * ('a -> json) * 'a -> of_json
  let of_json_cases = [
    C ({|1|}, user_of_json, user_to_json, 1);
    C ({|"OK"|}, (param_of_json string_of_json), (param_to_json string_to_json), "OK");
    C ({|"some"|}, opt_of_json, opt_to_json, (Some "some"));
    C ({|[42, "works"]|}, tuple_of_json, tuple_to_json, (42, "works"));
    C ({|{"name":"N","age":1}|}, record_of_json, record_to_json, {name="N"; age=1});
    C ({|["A"]|}, sum_of_json, sum_to_json, (A : sum));
    C ({|["B", 42]|}, sum_of_json, sum_to_json, (B 42 : sum));
    C ({|["C", {"name": "cname"}]|}, sum_of_json, sum_to_json, (C {name="cname"} : sum));
    C ({|["A"]|}, poly_of_json, poly_to_json, (`A : poly));
    C ({|["B", 42]|}, poly_of_json, poly_to_json, (`B 42 : poly));
    C ({|["Fix",["Fix",["Fix",["A"]]]]|}, recur_of_json, recur_to_json, (Fix (Fix (Fix A))));
    C ({|["Fix",["Fix",["Fix",["A"]]]]|}, polyrecur_of_json, polyrecur_to_json, (`Fix (`Fix (`Fix `A))));
  ]
  let run' ~json_of_string ~json_to_string (C (data, of_json, to_json, v)) =
    print_endline (Printf.sprintf "JSON    DATA: %s" data);
    let json = json_of_string data in
    let v' = of_json json in
    assert (v' = v);
    let json' = to_json v' in
    let data' = json_to_string json' in
    print_endline (Printf.sprintf "JSON REPRINT: %s" data')
  let run ~json_of_string ~json_to_string () =
    List.iter (run' ~json_of_string ~json_to_string) of_json_cases
end

