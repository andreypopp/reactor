type world = { lab : string; opt : int option; name : string }
[@@deriving yojson]

type greeting = Greeting_formal | Greeting_informal [@@deriving yojson]

module type Hello = sig
  val hello : name:string -> string Promise.t [@@query]
  val update_greeting : greeting:greeting -> unit Promise.t [@@mutation]

  val world : lab:string -> ?opt:int -> string -> world Promise.t
  [@@query]
end
[@@deriving remote { path = "/hello" }]

type todo = { id : int; text : string; completed : bool }
[@@deriving yojson]

module type Todo = sig
  val list : unit -> todo list Promise.t [@@query]
  (** List all todods. *)

  val create : text:string -> unit -> todo Promise.t
  [@@mutation]
  (** Create new todo. *)

  val update :
    ?text:string ->
    ?completed:bool ->
    id:int ->
    unit ->
    todo option Promise.t
  [@@mutation]
  (** Update a todo. *)

  val remove_completed : unit -> unit Promise.t
  [@@mutation]
  (** Remove completed todos. *)
end
[@@deriving remote { path = "/todo" }]
