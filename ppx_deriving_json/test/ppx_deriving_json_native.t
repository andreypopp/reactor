  $ alias run='../native/ppx_deriving_json_native_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF
  type user = int [@@deriving json]
  
  include struct
    let _ = fun (_ : user) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_of_json = (fun x -> int_of_json x : Yojson.Basic.t -> user)
    let _ = user_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_to_json = (fun x -> int_to_json x : user -> Yojson.Basic.t)
    let _ = user_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a param = 'a [@@deriving json]
  > EOF
  type 'a param = 'a [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a param) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_of_json a_of_json : Yojson.Basic.t -> 'a param =
     fun x -> a_of_json x
  
    let _ = param_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_to_json a_to_json : 'a param -> Yojson.Basic.t =
     fun x -> a_to_json x
  
    let _ = param_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type opt = string option [@@deriving json]
  > EOF
  type opt = string option [@@deriving json]
  
  include struct
    let _ = fun (_ : opt) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_of_json =
      (fun x -> (option_of_json string_of_json) x : Yojson.Basic.t -> opt)
  
    let _ = opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_to_json =
      (fun x -> (option_to_json string_to_json) x : opt -> Yojson.Basic.t)
  
    let _ = opt_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type tuple = int * string [@@deriving json]
  > EOF
  type tuple = int * string [@@deriving json]
  
  include struct
    let _ = fun (_ : tuple) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_of_json =
      (fun x ->
         match x with
         | `List [ x_0; x_1 ] -> int_of_json x_0, string_of_json x_1
         | _ ->
             Ppx_deriving_json_runtime.of_json_error
               "expected a JSON array of length 2"
        : Yojson.Basic.t -> tuple)
  
    let _ = tuple_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_to_json =
      (fun x ->
         match x with
         | x_0, x_1 -> `List [ int_to_json x_0; string_to_json x_1 ]
        : tuple -> Yojson.Basic.t)
  
    let _ = tuple_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type record = { name : string; age : int } [@@deriving json]
  > EOF
  type record = { name : string; age : int } [@@deriving json]
  
  include struct
    let _ = fun (_ : record) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_of_json =
      (fun x ->
         match x with
         | `Assoc fs ->
             let x_name = ref Stdlib.Option.None in
             let x_age = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "name" ->
                       x_name := Stdlib.Option.Some (string_of_json v)
                   | "age" -> x_age := Stdlib.Option.Some (int_of_json v)
                   | name ->
                       Ppx_deriving_json_runtime.of_json_error
                         (Stdlib.Printf.sprintf "unknown field: %s" name));
                   iter fs
             in
             iter fs;
             {
               name =
                 (match Stdlib.( ! ) x_name with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Ppx_deriving_json_runtime.of_json_error
                       "missing field \"name\"");
               age =
                 (match Stdlib.( ! ) x_age with
                 | Stdlib.Option.Some v -> v
                 | Stdlib.Option.None ->
                     Ppx_deriving_json_runtime.of_json_error
                       "missing field \"age\"");
             }
         | _ ->
             Ppx_deriving_json_runtime.of_json_error
               "expected a JSON object"
        : Yojson.Basic.t -> record)
  
    let _ = record_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             `Assoc
               [ "name", string_to_json x_name; "age", int_to_json x_age ]
        : record -> Yojson.Basic.t)
  
    let _ = record_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type sum = A | B of int | C of { name : string } [@@deriving json]
  > EOF
  type sum = A | B of int | C of { name : string } [@@deriving json]
  
  include struct
    let _ = fun (_ : sum) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) -> A
         | `List [ `String "B"; x_0 ] -> B (int_of_json x_0)
         | `List [ `String "C"; `Assoc fs ] ->
             let x_name = ref Stdlib.Option.None in
             let rec iter = function
               | [] -> ()
               | (n', v) :: fs ->
                   (match n' with
                   | "name" ->
                       x_name := Stdlib.Option.Some (string_of_json v)
                   | name ->
                       Ppx_deriving_json_runtime.of_json_error
                         (Stdlib.Printf.sprintf "unknown field: %s" name));
                   iter fs
             in
             iter fs;
             C
               {
                 name =
                   (match Stdlib.( ! ) x_name with
                   | Stdlib.Option.Some v -> v
                   | Stdlib.Option.None ->
                       Ppx_deriving_json_runtime.of_json_error
                         "missing field \"name\"");
               }
         | _ -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Yojson.Basic.t -> sum)
  
    let _ = sum_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_to_json =
      (fun x ->
         match x with
         | A -> `List [ `String "A" ]
         | B x_0 -> `List [ `String "B"; int_to_json x_0 ]
         | C { name = x_name } ->
             `List [ `String "C"; `Assoc [ "name", string_to_json x_name ] ]
        : sum -> Yojson.Basic.t)
  
    let _ = sum_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type other = [ `C ] [@@deriving json] type poly = [ `A | `B of int | other ] [@@deriving json]
  > EOF
  type other = [ `C ] [@@deriving json]
  
  include struct
    let _ = fun (_ : other) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_of_json_poly =
      (fun x ->
         match x with `List (`String "C" :: []) -> Some `C | x -> None
        : Yojson.Basic.t -> other option)
  
    and other_of_json =
      (fun x ->
         match other_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Yojson.Basic.t -> other)
  
    let _ = other_of_json_poly
    and _ = other_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_to_json =
      (fun x -> match x with `C -> `List [ `String "C" ]
        : other -> Yojson.Basic.t)
  
    let _ = other_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
  type poly = [ `A | `B of int | other ] [@@deriving json]
  
  include struct
    let _ = fun (_ : poly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_of_json_poly =
      (fun x ->
         match x with
         | `List (`String "A" :: []) -> Some `A
         | `List [ `String "B"; x_0 ] -> Some (`B (int_of_json x_0))
         | x -> (
             match other_of_json_poly x with
             | Some x -> (Some x :> [ `A | `B of int | other ] option)
             | None -> None)
        : Yojson.Basic.t -> poly option)
  
    and poly_of_json =
      (fun x ->
         match poly_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Yojson.Basic.t -> poly)
  
    let _ = poly_of_json_poly
    and _ = poly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_to_json =
      (fun x ->
         match x with
         | `A -> `List [ `String "A" ]
         | `B x_0 -> `List [ `String "B"; int_to_json x_0 ]
         | #other as x -> other_to_json x
        : poly -> Yojson.Basic.t)
  
    let _ = poly_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a c = [ `C of 'a ] [@@deriving json]
  > EOF
  type 'a c = [ `C of 'a ] [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a c) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_of_json_poly a_of_json : Yojson.Basic.t -> 'a c option =
     fun x ->
      match x with
      | `List [ `String "C"; x_0 ] -> Some (`C (a_of_json x_0))
      | x -> None
  
    and c_of_json a_of_json : Yojson.Basic.t -> 'a c =
     fun x ->
      match (c_of_json_poly a_of_json) x with
      | Some x -> x
      | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
  
    let _ = c_of_json_poly
    and _ = c_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_to_json a_to_json : 'a c -> Yojson.Basic.t =
     fun x -> match x with `C x_0 -> `List [ `String "C"; a_to_json x_0 ]
  
    let _ = c_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type recur = A | Fix of recur [@@deriving json]
  > EOF
  type recur = A | Fix of recur [@@deriving json]
  
  include struct
    let _ = fun (_ : recur) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_of_json =
      (fun x ->
         match x with
         | `List (`String "A" :: []) -> A
         | `List [ `String "Fix"; x_0 ] -> Fix (recur_of_json x_0)
         | _ -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Yojson.Basic.t -> recur)
  
    let _ = recur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_to_json =
      (fun x ->
         match x with
         | A -> `List [ `String "A" ]
         | Fix x_0 -> `List [ `String "Fix"; recur_to_json x_0 ]
        : recur -> Yojson.Basic.t)
  
    let _ = recur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
  > EOF
  type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving json]
  
  include struct
    let _ = fun (_ : polyrecur) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_of_json_poly =
      (fun x ->
         match x with
         | `List (`String "A" :: []) -> Some `A
         | `List [ `String "Fix"; x_0 ] ->
             Some (`Fix (polyrecur_of_json x_0))
         | x -> None
        : Yojson.Basic.t -> polyrecur option)
  
    and polyrecur_of_json =
      (fun x ->
         match polyrecur_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Yojson.Basic.t -> polyrecur)
  
    let _ = polyrecur_of_json_poly
    and _ = polyrecur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_to_json =
      (fun x ->
         match x with
         | `A -> `List [ `String "A" ]
         | `Fix x_0 -> `List [ `String "Fix"; polyrecur_to_json x_0 ]
        : polyrecur -> Yojson.Basic.t)
  
    let _ = polyrecur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

