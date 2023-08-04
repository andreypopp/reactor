open React

let%component sidebar ~title children =
  let () = use_effect' (fun () -> print_endline "effect") [||] in
  div ~className:"some" [| text title; text "WORLD!!"; div children |]

let%component wait_and_print ~promise ~msg =
  let () = use promise in
  text msg

module%export_component App = struct
  type props = { title : string; children : element }

  let make props =
    let promise = Promise.sleep 1.0 in
    div
      [|
        h1 [| text props.title; text "!!!" |];
        sidebar ~title:"sidebar" [||];
        div ~className:"footer" [| props.children; props.children |];
        suspense [| wait_and_print ~promise ~msg:"SLEPT" |];
      |]
end

(* let () = *)
(*   let app = sidebar ~title:"title" [| text "body"; text "another" |] in *)
(*   print_endline (render_to_string app) *)
