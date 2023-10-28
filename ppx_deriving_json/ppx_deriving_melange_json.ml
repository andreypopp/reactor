let () =
  let _ = Ppx_deriving_schema.register Of_melange_json.of_json in
  let _ = Ppx_deriving_schema.register To_melange_json.to_json in
  ()
