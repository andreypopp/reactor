type 'a promise = 'a Js_promise.t

let sleep sec =
  Js_promise.make @@ fun ~resolve ~reject:_ ->
  let unit = () in
  ignore
    (Js_global.setIntervalFloat
       (fun () -> (resolve unit [@bs]))
       (sec *. 1000.))
