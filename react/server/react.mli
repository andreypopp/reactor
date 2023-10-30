include
  React_api.REACT
    with type element = React_server.React.element
     and type 'a promise = 'a Promise.t

module Suspense : sig
  val make :
    ?key:string ->
    ?fallback:element ->
    children:element ->
    unit ->
    element
end
