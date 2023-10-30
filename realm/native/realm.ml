module Promise = struct
  type 'a t = 'a Lwt.t

  let return v = Lwt.return v
  let ( let* ) v e = Lwt.bind v e
  let sleep = Lwt_unix.sleep
end

module Json = struct
  type t = Yojson.Basic.t

  let to_json t = t
  let of_json t = t

  module To_json = struct
    let string_to_json v = `String v
    let bool_to_json v = `Bool v
    let int_to_json v = `Int v
    let unit_to_json () = `Null
    let list_to_json v_to_json vs = `List (List.map v_to_json vs)

    let option_to_json v_to_json = function
      | None -> `Null
      | Some v -> v_to_json v
  end

  exception Of_json_error of string

  let of_json_error msg = raise (Of_json_error msg)

  module Of_json = struct
    let string_of_json = Yojson.Basic.Util.to_string
    let bool_of_json = Yojson.Basic.Util.to_bool
    let int_of_json = Yojson.Basic.Util.to_int

    let unit_of_json = function
      | `Null -> ()
      | _ -> of_json_error "expected null"

    let option_of_json v_of_json = Yojson.Basic.Util.to_option v_of_json
    let list_of_json v_of_json = Yojson.Basic.Util.to_list v_of_json
  end
end

include Json.To_json
include Json.Of_json
