include
  React_api.REACT
    with type element = React_server.React.element
     and type 'a promise = 'a Promise.t

val useOptimistic :
  'state ->
  (_ -> React_server.browser_only) ->
  'state * (_ -> React_server.browser_only)
