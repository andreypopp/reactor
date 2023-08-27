open! ContainersLabels
open! Monomorphic
open Printf
open Lwt.Infix
include Api_spec

let current_greeting = ref "Hello"

module Hello = Hello_make (struct
  let hello ~name =
    Lwt.pause () >>= fun () ->
    Lwt.return
      (sprintf "%s, %s" !current_greeting (String.capitalize_ascii name))

  let update_greeting ~greeting =
    (current_greeting :=
       match greeting with
       | Greeting_formal -> "Hello"
       | Greeting_informal -> "HIIII");
    Lwt.return ()

  let world ~lab ?opt name = Lwt.return { name; lab; opt }
end)

module Todo = Todo_make (struct
  type db = { seq : int ref; todos : (int, todo) Hashtbl.t }

  let db = { seq = ref 0; todos = Hashtbl.create 10 }
  let list () = Lwt.return (Hashtbl.values_list db.todos)

  let create ~text () =
    let id = Ref.get_then_incr db.seq in
    let todo = { id; text; completed = false } in
    Hashtbl.replace db.todos todo.id todo;
    Lwt.return todo

  let update ?text ?completed ~id () =
    match Hashtbl.find_opt db.todos id with
    | None -> Lwt.return_none
    | Some todo ->
        Hashtbl.replace db.todos id
          {
            todo with
            text = Option.value text ~default:todo.text;
            completed = Option.value completed ~default:todo.completed;
          };
        Lwt.return_some todo

  let remove_completed () =
    Hashtbl.filter_map_inplace
      (fun _id todo ->
        match todo.completed with true -> None | false -> Some todo)
      db.todos;
    Lwt.return ()
end)
