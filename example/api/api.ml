open Printf
open Lwt.Infix
open Api_spec

let current_greeting = ref "Hello"

include Hello_make (struct
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
