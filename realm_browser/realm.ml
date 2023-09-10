module Promise = struct
  type 'a t = 'a Js.Promise.t

  let return v = Js.Promise.resolve v
  let ( let* ) v e = Js.Promise.then_ e v

  let sleep sec =
    Js.Promise.make @@ fun ~resolve ~reject:_ ->
    let unit = () in
    ignore
      (Js.Global.setIntervalFloat
         (fun () -> (resolve unit [@bs]))
         (sec *. 1000.))
end
