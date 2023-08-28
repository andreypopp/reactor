include React_server.React

let use_effect0 _thunk = ()
let use_effect1 _a _thunk = ()
let use_effect2 _a _b _thunk = ()
let use_effect0' _thunk = ()
let use_effect1' _a _thunk = ()
let use_effect2' _a _b _thunk = ()
let use_layout_effect0 _thunk = ()
let use_layout_effect1 _a _thunk = ()
let use_layout_effect2 _a _b _thunk = ()
let use_layout_effect0' _thunk = ()
let use_layout_effect1' _a _thunk = ()
let use_layout_effect2' _a _b _thunk = ()

type 'a promise = 'a Lwt.t

let use promise =
  match Lwt.state promise with
  | Return v -> v
  | Sleep -> raise_notrace (Suspend (Any_promise promise))
  | Fail exn -> raise exn
