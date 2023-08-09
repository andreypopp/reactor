[@@@warning "-32-27-26"]

open React

let%component counter ~title =
  let v, setv = use_state (Fun.const 0) in
  let succ () = setv Int.succ in
  let reset () = setv (Fun.const 0) in
  jsx.div ~className:"pa4"
    [|
      jsx.h2 [| text title |];
      jsx.p [| textf "clicked %i times" v |];
      jsx.button ~onClick:succ [| text "Increment" |];
      jsx.button ~onClick:reset [| text "Reset" |];
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
        counter ~title:"Counter";
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
