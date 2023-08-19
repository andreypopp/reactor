type world = { lab : string; opt : int option; name : string }
[@@deriving yojson]

module type%rpcgen Hello = sig
  val hello : name:string -> string Lwt.t
  val world : lab:string -> ?opt:int -> string -> world Lwt.t
end
