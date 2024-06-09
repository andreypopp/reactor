open ContainersLabels
open Lwt.Infix
open Routing.Api

module Hello = Remote.Make (struct
  include Hello

  let current_greeting = ref "Hello"
  let href route = Routing.href (Routing.Api_hello route)

  let handle : type a. a Hello.t -> a Lwt.t =
   fun route ->
    match route with
    | Hello { name } ->
        Lwt.pause () >>= fun () ->
        Lwt.return
          (Printf.sprintf "%s, %s" !current_greeting
             (String.capitalize_ascii name))
    | Update_greeting { greeting } ->
        (current_greeting :=
           match greeting with
           | Greeting_formal -> "Hello"
           | Greeting_informal -> "HIIII");
        Lwt.return ()
end)

module Todo = Remote.Make (struct
  include Todo

  let href route = Routing.href (Routing.Api_todo route)

  type db = { seq : int ref; todos : (int, todo) Hashtbl.t }

  let db = { seq = ref 0; todos = Hashtbl.create 10 }

  let handle : type a. a Todo.t -> a Lwt.t =
   fun route ->
    match route with
    | List -> Lwt.return (Hashtbl.values_list db.todos)
    | Create { text } ->
        let id = Ref.get_then_incr db.seq in
        let todo = { id; text; completed = false } in
        Hashtbl.replace db.todos todo.id todo;
        Lwt.return todo
    | Update { id; text; completed } -> (
        match Hashtbl.find_opt db.todos id with
        | None -> Lwt.return_none
        | Some todo ->
            Hashtbl.replace db.todos id
              {
                todo with
                text = Option.value text ~default:todo.text;
                completed = Option.value completed ~default:todo.completed;
              };
            Lwt.return_some todo)
    | Remove_completed ->
        Hashtbl.filter_map_inplace
          (fun _id (todo : todo) ->
            match todo.completed with true -> None | false -> Some todo)
          db.todos;
        Lwt.return ()
end)
