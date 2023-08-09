[@@@warning "-32-27-26"]

open React

let%component sidebar ~title children =
  let v, setv = use_state (fun () -> 0) in
  let () = use_effect' (fun () -> [%browser_only Js.log "effect"]) [||] in
  let onClick () =
    [%browser_only
      Js.log "OK";
      setv (fun v -> v + 1)]
  in
  jsx.div ~className:"some"
    [|
      text title;
      jsx.button ~onClick [| text "HELLO" |];
      textf "Counter: %i" v;
      jsx.div children;
    |]

let%component wait_and_print ~promise ?promise2 msg =
  let () = use promise in
  let () = Option.map use promise2 |> Option.value ~default:() in
  jsx.li [| text msg |]

module%export_component App = struct
  type props = { title : string; children : element }

  let make props =
    let promise = Promise.sleep 1.0 in
    let promise2 = Promise.sleep 2.0 in
    let promise_inner = Promise.sleep 0.5 in
    jsx.div
      [|
        jsx.h2 [| textf "Hello, %s!" props.title |];
        sidebar ~title:"sidebar" [||];
        jsx.div ~className:"footer" [| props.children; props.children |];
        jsx.ul
          [|
            suspense
              [|
                suspense
                  [|
                    wait_and_print ~promise:promise_inner "INNER SLEPT";
                  |];
                suspense
                  [|
                    wait_and_print ~promise:promise_inner "INNER SLEPT";
                  |];
                wait_and_print ~promise ~promise2 "SLEPT";
                wait_and_print ~promise ~promise2 "SLEPT";
              |];
          |];
      |]
end

(* let%browser_only () = *)
(*   let app = sidebar ~title:"title" [| text "body"; text "another" |] in *)
(*   print_endline (render_to_string app) *)
