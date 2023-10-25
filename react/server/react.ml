include React_server.React

let useEffect _thunk = ()
let useEffect1 _thunk _deps = ()

type 'a promise = 'a Lwt.t

let use promise =
  match Lwt.state promise with
  | Return v -> v
  | Sleep -> raise_notrace (Suspend (Any_promise promise))
  | Fail exn -> raise exn
