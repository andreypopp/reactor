open Printf
open Lwt.Infix

let greeting = ref "Hello"

include Api_spec.Hello_make (struct
  let hello ~name =
    Lwt.pause () >>= fun () ->
    Lwt.return (sprintf "%s, %s" !greeting (String.capitalize_ascii name))

  let update_greeting ~greeting:v =
    greeting := v;
    Lwt.return ()

  let world ~lab ?opt name = Lwt.return { Api_spec.name; lab; opt }
end)
