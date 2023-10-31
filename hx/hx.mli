type trigger = Hx_runtime.trigger =
  | On_click
  | On_submit
  | On_timer of int

val trigger_of_json : React_server.json -> trigger
val trigger_to_json : trigger -> React_server.json

module Form : sig
  val make :
    ?key:string ->
    id:string ->
    children:React.element ->
    unit ->
    React.element
end

module Target : sig
  val make :
    ?key:string ->
    id:string ->
    children:React.element ->
    unit ->
    React.element
end

module Request : sig
  val make :
    ?key:string ->
    ?params:(string * React_server.json) list ->
    ?params_target:string ->
    ?target:string ->
    path:string ->
    trigger:trigger ->
    children:React.element ->
    unit ->
    React.element
end

val hx_runtime : unit -> Dream.route
val hx_page : string -> (Dream.request -> React.element) -> Dream.route

val hx_handle :
  Dream.method_ ->
  string ->
  (Dream.request -> React.element) ->
  Dream.route
