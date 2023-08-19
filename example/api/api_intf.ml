[@@@ocaml.warning "-101"] (* TODO inject via ppx? *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type world = { lab : string; opt : int option; name : string }
[@@deriving yojson]

module type%rpcgen Hello = sig
  val hello : name:string -> string Lwt.t

  val world :
    lab:string -> ?opt:int -> (string[@rpcgen.as "name"]) -> world Lwt.t
end
