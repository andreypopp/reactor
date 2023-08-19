type world = { lab : string; opt : int option; name : string }
[@@deriving yojson]

module type Hello = sig
  val hello : name:string -> string Promise.t
  val world : lab:string -> ?opt:int -> string -> world Promise.t
end
[@@deriving rpcgen]
