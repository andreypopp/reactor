let () =
  let _ = Ppx_deriving_schema.register Of_yojson.of_json in
  let _ = Ppx_deriving_schema.register To_yojson.to_json in
  ()
