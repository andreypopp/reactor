module Promise = struct
  type 'a t = 'a Lwt.t
  let return v = Lwt.return v
  let ( let* ) v e = Lwt.bind v e
  let sleep = Lwt_unix.sleep
end

