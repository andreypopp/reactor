open Printf
open Lwt.Infix

include Api_intf.Hello_make (struct
  let hello ~name =
    Lwt.pause () >>= fun () ->
    Lwt.return (sprintf "%s" (String.capitalize_ascii name))

  let world ~lab ?opt name = Lwt.return { Api_intf.name; lab; opt }
end)
