type world = { lab : string; opt : int option; name : string }
[@@deriving yojson]

module type Hello = sig
  val hello : name:string -> string Promise.t [@@query]
  val update_greeting : greeting:string -> unit Promise.t [@@mutation]

  val world : lab:string -> ?opt:int -> string -> world Promise.t
  [@@query]
end
[@@deriving remote]
