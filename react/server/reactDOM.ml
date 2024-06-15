module Ref = struct
  type domRef = DomRef

  let useCurrentDomRef () =
    let ref = React.useRef None in
    ref, DomRef
end
