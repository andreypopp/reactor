[@@@warning "-32-27-26"]

let%component link ~href label =
  let%browser_only onClick ev =
    React_browser.Event.preventDefault ev;
    React_browser.navigate href
  in
  jsx.a ~href ~onClick [| React.text label |]

let%component button ~onPress:onClick label =
  jsx.button ~className:"pv1 ph2 br1 bg-light-gray bw1 b--gray" ~onClick
    [| React.text label |]

let%component hello ~name () =
  let q, setq = React.use_state (fun () -> Api.Hello.hello ~name) in
  let () =
    React.use_effect1' name @@ fun () ->
    React.start_transition @@ fun () ->
    setq (fun _ -> Api.Hello.hello ~name)
  in
  let msg = React.use (Remote.run_query q) in
  let%browser_only onClick _ev =
    ignore
      (Promise.(
         let* () =
           Remote.run_mutation
           @@ Api.Hello.update_greeting ~greeting:Api.Greeting_informal
         in
         Remote.invalidate (Api.Hello.hello ~name);
         ( React.start_transition @@ fun () ->
           setq (fun _ -> Api.Hello.hello ~name) );
         return ())
        : unit Promise.t)
  in
  jsx.div ~onClick [| React.text msg |]

let%component counter ~init ~title () =
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
      hello ~name:(Printf.sprintf "hello #%i" v) ();
    |]

let%component wait_and_print ~promise ?promise2 msg =
  let () = React.use promise in
  let () = Option.map React.use promise2 |> Option.value ~default:() in
  jsx.li [| React.text msg |]

let%component nav () =
  jsx.ul
    [|
      jsx.li [| link ~href:"/" "Main page" |];
      jsx.li [| link ~href:"/todo" "TODOs" |];
      jsx.li [| link ~href:"/about" "About" |];
    |]

type about_mode = About_light | About_dark [@@deriving yojson]

let%export_component about ~(mode : about_mode) ~(num : int) () =
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
    React.use_effect0' (fun () -> Js.log "HELLO, I'M READY")
  in
  jsx.div
    [|
      nav ();
      React.suspense
        [| hello ~name:"world" (); hello ~name:"something else" () |];
      jsx.h2 [| React.textf "Hello, %s!" title |];
      React.suspense [| counter ~init:42 ~title:"Counter" () |];
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

let%component todo_item ~on_completed todo =
  let%browser_only onChange _ev =
    on_completed todo (not todo.Api.completed)
  in
  jsx.li ~className:"ma0 pa0 flex items-center"
    [|
      jsx.label
        [|
          jsx.input ~className:"scale3" ~type_:"checkbox"
            ~checked:todo.Api.completed ~onChange;
          jsx.span ~className:"pl2" [| React.text todo.Api.text |];
        |];
    |]

let%component todo_section ~title ~on_completed todos =
  let todos =
    List.map
      (fun todo ->
        let key = Int.to_string todo.Api.id in
        todo_item ~key ~on_completed todo)
      todos
  in
  jsx.div ~className:"pb2"
    [|
      jsx.h5 ~className:"ma0 pv2" [| React.text title |];
      jsx.ul ~className:"ma0 pv0 ph2 flex flex-column g1"
        (Array.of_list todos);
    |]

let%component add_todo_form ~on_create () =
  let value, set_value = React.use_state (fun () -> "") in
  let input, set_input = React.use_dom_ref () in
  let%browser_only onChange ev =
    let value = React_browser.Event.(target ev |> Target.value_exn) in
    set_value (fun _ -> value)
  in
  let%browser_only create () =
    ignore
      Promise.(
        let* () = on_create value in
        set_value (fun _ -> "");
        Option.iter Webapi.Dom.HtmlElement.focus (React.deref input);
        return ())
  in
  let%browser_only onKeyDown ev =
    match React_browser.Event.Keyboard.key ev with
    | "Enter" -> create ()
    | _ -> ()
  in
  let%browser_only onClick _ev = create () in
  jsx.div
    [|
      jsx.input ~ref:set_input ~onChange ~onKeyDown ~type_:"text" ~value;
      jsx.button ~onClick [| React.text "Add" |];
    |]

let%component todo_list' () =
  let todos, set_todos = React.use_state Api.Todo.list in
  let todos = React.use (Remote.run_query todos) in
  let%browser_only refetch () =
    React.start_transition @@ fun () ->
    let%browser_only () = Remote.invalidate (Api.Todo.list ()) in
    set_todos (fun _ -> Api.Todo.list ())
  in
  let completed, to_be_done =
    List.partition_map
      (fun todo ->
        match todo.Api.completed with
        | true -> Left todo
        | false -> Right todo)
      todos
  in
  let%browser_only on_create text =
    Promise.(
      let* _new_todo = Remote.run_mutation @@ Api.Todo.create ~text () in
      refetch ();
      return ())
  in
  let%browser_only on_completed todo completed =
    ignore
      Promise.(
        let* _new_todo =
          Remote.run_mutation
          @@ Api.Todo.update ~completed ~id:todo.Api.id ()
        in
        refetch ();
        return ())
  in
  let%browser_only on_remove_completed _ev =
    ignore
      Promise.(
        let* _new_todo =
          Remote.run_mutation @@ Api.Todo.remove_completed ()
        in
        refetch ();
        return ())
  in
  jsx.div
    [|
      add_todo_form ~on_create ();
      todo_section ~title:"To be done" ~on_completed to_be_done;
      todo_section ~title:"Completed" ~on_completed completed;
      jsx.button ~onClick:on_remove_completed
        [| React.text "Remove completed todos" |];
    |]

let%export_component todo_list () = jsx.div [| nav (); todo_list' () |]

let%browser_only () =
  Js.log "this will execute only in browser on startup"
