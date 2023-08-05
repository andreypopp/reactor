[@@@warning "-32-27-26"]

open React

let%component sidebar ~title children =
  let () = use_effect' (fun () -> print_endline "effect") [||] in
  div ~className:"some" [| text title; text "WORLD!!"; div children |]

let%component wait_and_print ~promise ?promise2 msg =
  let () = use promise in
  let () =
    match promise2 with None -> () | Some promise -> use promise
  in
  text msg

module%export_component App = struct
  type props = { title : string; children : element }

  let make props =
    let promise = Promise.sleep 1.0 in
    let promise2 = Promise.sleep 2.0 in
    let promise_inner = Promise.sleep 0.5 in
    div
      [|
        h1 [| text props.title; text "!!!" |];
        sidebar ~title:"sidebar" [||];
        div ~className:"footer" [| props.children; props.children |];
        suspense
          [|
            suspense
              [| wait_and_print ~promise:promise_inner "INNER SLEPT" |];
            suspense
              [| wait_and_print ~promise:promise_inner "INNER SLEPT" |];
            wait_and_print ~promise ~promise2 "SLEPT";
            wait_and_print ~promise ~promise2 "SLEPT";
          |];
      |]
end

(* let () = *)
(*   let app = sidebar ~title:"title" [| text "body"; text "another" |] in *)
(*   print_endline (render_to_string app) *)
