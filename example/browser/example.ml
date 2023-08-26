[@@@warning "-32-27-26"]

let%component link ~href label =
  let%browser_only onClick ev =
    React.prevent_default ev;
    React.navigate href
  in
  jsx.a ~href ~onClick [| React.text label |]

let%component button ~onPress:onClick label =
  jsx.button ~className:"pv1 ph2 br1 bg-light-gray bw1 b--gray" ~onClick
    [| React.text label |]

let%component hello ~name =
  let q, setq = React.use_state (fun () -> Api.hello ~name) in
  React.use_effect'
    (fun () ->
      React.start_transition @@ fun () -> setq (fun _ -> Api.hello ~name))
    [| name |];
  let msg = React.use (Remote.run_query q) in
  let%browser_only onClick _ev =
    ignore
      (Promise.(
         let* () =
           Remote.run_mutation
           @@ Api.update_greeting ~greeting:Api.Greeting_informal
         in
         Remote.invalidate (Api.hello ~name);
         ( React.start_transition @@ fun () ->
           setq (fun _ -> Api.hello ~name) );
         return ())
        : unit Promise.t)
  in
  jsx.div ~onClick [| React.text msg |]

let%component counter ~init ~title =
  let v, setv = React.use_state (Fun.const init) in
  let succ _ev = React.start_transition @@ fun () -> setv Int.succ in
  let pred _ev = React.start_transition @@ fun () -> setv Int.pred in
  let reset _ev =
    React.start_transition @@ fun () -> setv (Fun.const 0)
  in
  jsx.div ~className:"pa4"
    [|
      jsx.h2 [| React.text title |];
      jsx.p [| React.textf "clicked %i times" v |];
      button ~onPress:succ "Increment";
      button ~onPress:pred "Decrement";
      button ~onPress:reset "Reset";
      hello ~name:(Printf.sprintf "hello #%i" v);
    |]

let%component wait_and_print ~promise ?promise2 msg =
  let () = React.use promise in
  let () = Option.map React.use promise2 |> Option.value ~default:() in
  jsx.li [| React.text msg |]

let%component nav () =
  jsx.ul
    [|
      jsx.li [| link ~href:"/" "Main page" |];
      jsx.li [| link ~href:"/about" "About" |];
    |]

type about_mode = About_light | About_dark [@@deriving yojson]

let%export_component about ~(mode : about_mode) ~(num : int) =
  let%browser_only () =
    match mode with
    | About_dark -> Js.log "dark"
    | About_light -> Js.log "light"
  in
  jsx.div [| nav () |]

let%export_component app ~(title : string) (children : React.element) =
  let promise = Promise.sleep 1.0 in
  let promise2 = Promise.sleep 2.0 in
  let promise_inner = Promise.sleep 0.5 in
  let%browser_only () =
    React.use_effect' (fun () -> Js.log "HELLO, I'M READY") [||]
  in
  jsx.div
    [|
      nav ();
      React.suspense
        [| hello ~name:"world"; hello ~name:"something else" |];
      jsx.h2 [| React.textf "Hello, %s!" title |];
      React.suspense [| counter ~init:42 ~title:"Counter" |];
      jsx.div ~className:"footer" [| children; children |];
      jsx.ul
        [|
          React.suspense
            [|
              React.suspense
                [| wait_and_print ~promise:promise_inner "INNER SLEPT" |];
              React.suspense
                [| wait_and_print ~promise:promise_inner "INNER SLEPT" |];
              wait_and_print ~promise ~promise2 "SLEPT";
              wait_and_print ~promise ~promise2 "SLEPT";
            |];
        |];
    |]

let%browser_only () =
  Js.log "this will execute only in browser on startup"
