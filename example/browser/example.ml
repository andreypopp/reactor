[@@@warning "-32-27-26"]

open React

let%component button ~onPress:onClick label =
  jsx.button ~className:"pv1 ph2 br1 bg-light-gray bw1 b--gray" ~onClick
    [| text label |]

let%component hello ~name =
  let msg = use (Remote.run (Api.hello ~name)) in
  jsx.div [| textf "Hello, %s!" msg |]

let%component counter ~init ~title =
  let v, setv = use_state (Fun.const init) in
  let succ () = start_transition @@ fun () -> setv Int.succ in
  let pred () = start_transition @@ fun () -> setv Int.pred in
  let reset () = start_transition @@ fun () -> setv (Fun.const 0) in
  jsx.div ~className:"pa4"
    [|
      jsx.h2 [| text title |];
      jsx.p [| textf "clicked %i times" v |];
      button ~onPress:succ "Increment";
      button ~onPress:pred "Decrement";
      button ~onPress:reset "Reset";
      hello ~name:(Printf.sprintf "hello #%i" v);
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
    let%browser_only () =
      use_effect' (fun () -> Js.log "HELLO, I'M READY") [||]
    in
    jsx.div
      [|
        suspense [| hello ~name:"world"; hello ~name:"something else" |];
        jsx.h2 [| textf "Hello, %s!" props.title |];
        suspense [| counter ~init:42 ~title:"Counter" |];
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

let%browser_only () =
  Js.log "this will execute only in browser on startup"
