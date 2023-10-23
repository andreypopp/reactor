include
  React_api.REACT
    with type element = React_server.React.element
     and type 'a promise = 'a Promise.t

val suspense :
  ?key:string ->
  ?fallback:children ->
  children:children ->
  unit ->
  element
