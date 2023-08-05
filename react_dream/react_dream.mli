open React_server

val render :
  ?enable_ssr:bool ->
  ?scripts:string list ->
  ?links:string list ->
  (Dream.request -> React.element) ->
  Dream.handler
(** Serve React application. *)
