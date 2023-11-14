  $ alias run='../browser/ppx_deriving_json_browser_test.exe -impl - | ocamlformat - --impl'

  $ cat <<"EOF" | run
  > type user = int [@@deriving json]
  > EOF
  type user = int [@@deriving json]
  
  include struct
    let _ = fun (_ : user) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_of_json = (fun x -> int_of_json x : Js.Json.t -> user)
    let _ = user_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec user_to_json = (fun x -> int_to_json x : user -> Js.Json.t)
    let _ = user_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a param = 'a [@@deriving json]
  > EOF
  type 'a param = 'a [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a param) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_of_json a_of_json : Js.Json.t -> 'a param =
     fun x -> a_of_json x
  
    let _ = param_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec param_to_json a_to_json : 'a param -> Js.Json.t =
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
      (fun x -> (option_of_json string_of_json) x : Js.Json.t -> opt)
  
    let _ = opt_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec opt_to_json =
      (fun x -> (option_to_json string_to_json) x : opt -> Js.Json.t)
  
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
         if
           Js.Array.isArray x
           && Js.Array.length (Obj.magic x : Js.Json.t array) = 2
         then
           let es = (Obj.magic x : Js.Json.t array) in
           ( int_of_json (Js.Array.unsafe_get es 0),
             string_of_json (Js.Array.unsafe_get es 1) )
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a JSON array of length 2"
        : Js.Json.t -> tuple)
  
    let _ = tuple_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec tuple_to_json =
      (fun x ->
         match x with
         | x_0, x_1 ->
             (Obj.magic [| int_to_json x_0; string_to_json x_1 |]
               : Js.Json.t)
        : tuple -> Js.Json.t)
  
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
         if
           not
             (Js.typeof x = "object"
             && (not (Js.Array.isArray x))
             && not ((Obj.magic x : 'a Js.null) == Js.null))
         then
           Ppx_deriving_json_runtime.of_json_error "expected a JSON object";
         let fs =
           (Obj.magic x
             : < name : Js.Json.t Js.undefined
               ; age : Js.Json.t Js.undefined >
               Js.t)
         in
         {
           name =
             (match Js.Undefined.toOption fs##name with
             | Stdlib.Option.Some v -> string_of_json v
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"name\"");
           age =
             (match Js.Undefined.toOption fs##age with
             | Stdlib.Option.Some v -> int_of_json v
             | Stdlib.Option.None ->
                 Ppx_deriving_json_runtime.of_json_error
                   "missing field \"age\"");
         }
        : Js.Json.t -> record)
  
    let _ = record_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec record_to_json =
      (fun x ->
         match x with
         | { name = x_name; age = x_age } ->
             (Obj.magic
                [%mel.obj
                  { name = string_to_json x_name; age = int_to_json x_age }]
               : Js.Json.t)
        : record -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if len > 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Js.typeof tag = "string" then
               let tag = (Obj.magic tag : string) in
               if tag = "A" then (
                 if len <> 1 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1";
                 A)
               else if tag = "B" then (
                 if len <> 2 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2";
                 B (int_of_json (Js.Array.unsafe_get array 1)))
               else if tag = "C" then (
                 if len <> 2 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2";
                 let fs = Js.Array.unsafe_get array 1 in
                 if
                   not
                     (Js.typeof fs = "object"
                     && (not (Js.Array.isArray fs))
                     && not ((Obj.magic fs : 'a Js.null) == Js.null))
                 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON object";
                 let fs =
                   (Obj.magic fs : < name : Js.Json.t Js.undefined > Js.t)
                 in
                 C
                   {
                     name =
                       (match Js.Undefined.toOption fs##name with
                       | Stdlib.Option.Some v -> string_of_json v
                       | Stdlib.Option.None ->
                           Ppx_deriving_json_runtime.of_json_error
                             "missing field \"name\"");
                   })
               else Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
        : Js.Json.t -> sum)
  
    let _ = sum_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec sum_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic [| string_to_json "A" |] : Js.Json.t)
         | B x_0 ->
             (Obj.magic [| string_to_json "B"; int_to_json x_0 |]
               : Js.Json.t)
         | C { name = x_name } ->
             (Obj.magic
                [|
                  string_to_json "C";
                  (Obj.magic [%mel.obj { name = string_to_json x_name }]
                    : Js.Json.t);
                |]
               : Js.Json.t)
        : sum -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if len > 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Js.typeof tag = "string" then
               let tag = (Obj.magic tag : string) in
               if tag = "C" then (
                 if len <> 1 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1";
                 Some `C)
               else None
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
        : Js.Json.t -> other option)
  
    and other_of_json =
      (fun x ->
         match other_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Js.Json.t -> [ `C ])
  
    let _ = other_of_json_poly
    and _ = other_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec other_to_json =
      (fun x ->
         match x with
         | `C -> (Obj.magic [| string_to_json "C" |] : Js.Json.t)
        : other -> Js.Json.t)
  
    let _ = other_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
  type poly = [ `A | `B of int | other ] [@@deriving json]
  
  include struct
    let _ = fun (_ : poly) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_of_json_poly =
      (fun x ->
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if len > 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Js.typeof tag = "string" then
               let tag = (Obj.magic tag : string) in
               if tag = "A" then (
                 if len <> 1 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1";
                 Some `A)
               else if tag = "B" then (
                 if len <> 2 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2";
                 Some (`B (int_of_json (Js.Array.unsafe_get array 1))))
               else
                 match other_of_json_poly x with
                 | Some x -> (Some x :> [ `A | `B of int | other ] option)
                 | None -> None
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
        : Js.Json.t -> poly option)
  
    and poly_of_json =
      (fun x ->
         match poly_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Js.Json.t -> [ `A | `B of int | other ])
  
    let _ = poly_of_json_poly
    and _ = poly_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec poly_to_json =
      (fun x ->
         match x with
         | `A -> (Obj.magic [| string_to_json "A" |] : Js.Json.t)
         | `B x_0 ->
             (Obj.magic [| string_to_json "B"; int_to_json x_0 |]
               : Js.Json.t)
         | #other as x -> other_to_json x
        : poly -> Js.Json.t)
  
    let _ = poly_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  $ cat <<"EOF" | run
  > type 'a c = [ `C of 'a ] [@@deriving json]
  > EOF
  type 'a c = [ `C of 'a ] [@@deriving json]
  
  include struct
    let _ = fun (_ : 'a c) -> ()
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_of_json_poly a_of_json : Js.Json.t -> 'a c option =
     fun x ->
      if Js.Array.isArray x then
        let array = (Obj.magic x : Js.Json.t array) in
        let len = Js.Array.length array in
        if len > 0 then
          let tag = Js.Array.unsafe_get array 0 in
          if Js.typeof tag = "string" then
            let tag = (Obj.magic tag : string) in
            if tag = "C" then (
              if len <> 2 then
                Ppx_deriving_json_runtime.of_json_error
                  "expected a JSON array of length 2";
              Some (`C (a_of_json (Js.Array.unsafe_get array 1))))
            else None
          else
            Ppx_deriving_json_runtime.of_json_error
              "expected a non empty JSON array with element being a string"
        else
          Ppx_deriving_json_runtime.of_json_error
            "expected a non empty JSON array"
      else
        Ppx_deriving_json_runtime.of_json_error
          "expected a non empty JSON array"
  
    and c_of_json a_of_json : Js.Json.t -> [ `C of 'a ] =
     fun x ->
      match (c_of_json_poly a_of_json) x with
      | Some x -> x
      | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
  
    let _ = c_of_json_poly
    and _ = c_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec c_to_json a_to_json : 'a c -> Js.Json.t =
     fun x ->
      match x with
      | `C x_0 ->
          (Obj.magic [| string_to_json "C"; a_to_json x_0 |] : Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if len > 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Js.typeof tag = "string" then
               let tag = (Obj.magic tag : string) in
               if tag = "A" then (
                 if len <> 1 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1";
                 A)
               else if tag = "Fix" then (
                 if len <> 2 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2";
                 Fix (recur_of_json (Js.Array.unsafe_get array 1)))
               else Ppx_deriving_json_runtime.of_json_error "invalid JSON"
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
        : Js.Json.t -> recur)
  
    let _ = recur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec recur_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic [| string_to_json "A" |] : Js.Json.t)
         | Fix x_0 ->
             (Obj.magic [| string_to_json "Fix"; recur_to_json x_0 |]
               : Js.Json.t)
        : recur -> Js.Json.t)
  
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
         if Js.Array.isArray x then
           let array = (Obj.magic x : Js.Json.t array) in
           let len = Js.Array.length array in
           if len > 0 then
             let tag = Js.Array.unsafe_get array 0 in
             if Js.typeof tag = "string" then
               let tag = (Obj.magic tag : string) in
               if tag = "A" then (
                 if len <> 1 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 1";
                 Some `A)
               else if tag = "Fix" then (
                 if len <> 2 then
                   Ppx_deriving_json_runtime.of_json_error
                     "expected a JSON array of length 2";
                 Some
                   (`Fix (polyrecur_of_json (Js.Array.unsafe_get array 1))))
               else None
             else
               Ppx_deriving_json_runtime.of_json_error
                 "expected a non empty JSON array with element being a \
                  string"
           else
             Ppx_deriving_json_runtime.of_json_error
               "expected a non empty JSON array"
         else
           Ppx_deriving_json_runtime.of_json_error
             "expected a non empty JSON array"
        : Js.Json.t -> polyrecur option)
  
    and polyrecur_of_json =
      (fun x ->
         match polyrecur_of_json_poly x with
         | Some x -> x
         | None -> Ppx_deriving_json_runtime.of_json_error "invalid JSON"
        : Js.Json.t -> [ `A | `Fix of polyrecur ])
  
    let _ = polyrecur_of_json_poly
    and _ = polyrecur_of_json
  
    [@@@ocaml.warning "-39-11-27"]
  
    let rec polyrecur_to_json =
      (fun x ->
         match x with
         | `A -> (Obj.magic [| string_to_json "A" |] : Js.Json.t)
         | `Fix x_0 ->
             (Obj.magic [| string_to_json "Fix"; polyrecur_to_json x_0 |]
               : Js.Json.t)
        : polyrecur -> Js.Json.t)
  
    let _ = polyrecur_to_json
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

