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

  type db = { mutable seq : int; mutable todos : todo list }

  let db = { seq = 0; todos = [] }

  let slowly () =
    (* Simulate a slow database operation *)
    Lwt_unix.sleep 3.

  let handle : type a. a Todo.t -> a Lwt.t =
   fun route ->
    match route with
    | List -> Lwt.return db.todos
    | Create { text } ->
        slowly () >>= fun () ->
        let todo = { id = db.seq; text; completed = false } in
        db.seq <- db.seq + 1;
        db.todos <- todo :: db.todos;
        Lwt.return todo
    | Update { id; text; completed } ->
        slowly () >>= fun () ->
        let found = ref None in
        db.todos <-
          List.map db.todos ~f:(fun todo ->
              if Int.equal todo.id id then (
                let todo =
                  {
                    todo with
                    text = Option.value text ~default:todo.text;
                    completed =
                      Option.value completed ~default:todo.completed;
                  }
                in
                found := Some todo;
                todo)
              else todo);
        Lwt.return !found
    | Remove_completed ->
        slowly () >>= fun () ->
        db.todos <-
          List.filter db.todos ~f:(fun todo -> not todo.completed);
        Lwt.return ()
end)
