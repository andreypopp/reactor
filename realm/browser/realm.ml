module Promise = struct
  type 'a t = 'a Js.Promise.t

  let return v = Js.Promise.resolve v
  let ( let* ) v e = Js.Promise.then_ e v

  let sleep sec =
    Js.Promise.make @@ fun ~resolve ~reject:_ ->
    let unit = () in
    ignore
      (Js.Global.setIntervalFloat
         ~f:(fun () -> (resolve unit [@u]))
         (sec *. 1000.))
end

module Json = struct
  type t = Js.Json.t

  let to_json t = t
  let of_json t = t

  module To_json = struct
    external string_to_json : string -> t = "%identity"
    external bool_to_json : bool -> t = "%identity"
    external int_to_json : int -> t = "%identity"
    external float_to_json : float -> t = "%identity"

    let unit_to_json () : t = Obj.magic Js.null

    let list_to_json v_to_json vs : t =
      let vs = Array.of_list vs in
      let vs : Js.Json.t array = Array.map v_to_json vs in
      Obj.magic vs

    let option_to_json v_to_json v : t =
      match v with None -> Obj.magic Js.null | Some v -> v_to_json v
  end

  exception Of_json_error of string

  let of_json_error msg = raise (Of_json_error msg)

  module Of_json = struct
    let string_of_json (json : t) : string =
      if Js.typeof json = "string" then (Obj.magic json : string)
      else of_json_error "expected a string"

    let bool_of_json (json : t) : bool =
      if Js.typeof json = "boolean" then (Obj.magic json : bool)
      else of_json_error "expected a boolean"

    let is_int value =
      Js.Float.isFinite value && Js.Math.floor_float value == value

    let int_of_json (json : t) : int =
      if Js.typeof json = "number" then
        let v = (Obj.magic json : float) in
        if is_int v then (Obj.magic v : int)
        else of_json_error "expected an integer"
      else of_json_error "expected a boolean"

    let unit_of_json (json : t) =
      if (Obj.magic json : 'a Js.null) == Js.null then ()
      else of_json_error "expected null"

    let option_of_json v_of_json (json : t) =
      if (Obj.magic json : 'a Js.null) == Js.null then None
      else Some (v_of_json json)

    let list_of_json v_of_json (json : t) =
      if Js.Array.isArray json then
        let json = (Obj.magic json : Js.Json.t array) in
        Array.to_list (Array.map v_of_json json)
      else of_json_error "expected a JSON array"
  end
end

include Json.To_json
include Json.Of_json
