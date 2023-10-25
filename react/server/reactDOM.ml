module Ref = struct
  type domRef
end

type domRef = DomRef

let useDomRef () =
  let ref = React.useRef None in
  ref, DomRef
