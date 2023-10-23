(** Bindings to React event system

    Code in this module is heavily based on https://github.com/reasonml/reason-react

    MIT License
    Copyright (c) 2017 Sander
 *)

module Target = struct
  type t = Dom.eventTarget

  external value' : t -> Js.Json.t Js.null_undefined = "value" [@@mel.get]

  let value t =
    let v = value' t in
    match Js.Null_undefined.toOption v with
    | None -> None
    | Some v ->
        if Js.typeof v = "string" then Some (Obj.magic v : string)
        else None

  let value_exn t =
    match value t with
    | None -> failwith "target.value is undefined or not a string"
    | Some v -> v
end

type 'kind event

external bubbles : _ event -> bool = "bubbles" [@@mel.get]
external cancelable : _ event -> bool = "cancelable" [@@mel.get]

external currentTarget : _ event -> Target.t option = "currentTarget"
[@@mel.get] [@@mel.return nullable]

external defaultPrevented : _ event -> bool = "defaultPrevented"
[@@mel.get]

external eventPhase : _ event -> int = "eventPhase" [@@mel.get]
external isTrusted : _ event -> bool = "isTrusted" [@@mel.get]
external nativeEvent : _ event -> _ Js.t = "nativeEvent" [@@mel.get]
external preventDefault : _ event -> unit = "preventDefault" [@@mel.send]

external isDefaultPrevented : _ event -> bool = "isDefaultPrevented"
[@@mel.send]

external stopPropagation : _ event -> unit = "stopPropagation"
[@@mel.send]

external isPropagationStopped : _ event -> bool = "isPropagationStopped"
[@@mel.send]

external target : _ event -> Target.t = "target" [@@mel.get]
external timeStamp : _ event -> float = "timeStamp" [@@mel.get]
external type_ : _ event -> string = "type" [@@mel.get]
external persist : _ event -> unit = "persist" [@@mel.send]

type clipboard

module Clipboard = struct
  type t = clipboard event

  external clipboardData : t -> _ Js.t = "clipboardData" [@@mel.get]
end

type composition

module Composition = struct
  type t = composition event

  external data : t -> string = "data" [@@mel.get]
end

type keyboard

module Keyboard = struct
  type t = keyboard event

  external altKey : t -> bool = "altKey" [@@mel.get]
  external charCode : t -> int = "charCode" [@@mel.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@mel.get]
  external getModifierState : t -> bool = "getModifierState" [@@mel.send]
  external key : t -> string = "key" [@@mel.get]
  external keyCode : t -> int = "keyCode" [@@mel.get]
  external locale : t -> string = "locale" [@@mel.get]
  external location : t -> int = "location" [@@mel.get]
  external metaKey : t -> bool = "metaKey" [@@mel.get]
  external repeat : t -> bool = "repeat" [@@mel.get]
  external shiftKey : t -> bool = "shiftKey" [@@mel.get]
  external which : t -> int = "which" [@@mel.get]
end

type focus

module Focus = struct
  type t = focus event

  external relatedTarget : t -> Target.t option = "relatedTarget"
  [@@mel.get] [@@mel.return nullable]
end

type mouse

module Mouse = struct
  type t = mouse event

  external altKey : t -> bool = "altKey" [@@mel.get]
  external button : t -> int = "button" [@@mel.get]
  external buttons : t -> int = "buttons" [@@mel.get]
  external clientX : t -> int = "clientX" [@@mel.get]
  external clientY : t -> int = "clientY" [@@mel.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@mel.get]
  external getModifierState : t -> bool = "getModifierState" [@@mel.send]
  external metaKey : t -> bool = "metaKey" [@@mel.get]
  external movementX : t -> int = "movementX" [@@mel.get]
  external movementY : t -> int = "movementY" [@@mel.get]
  external pageX : t -> int = "pageX" [@@mel.get]
  external pageY : t -> int = "pageY" [@@mel.get]

  external relatedTarget : t -> Target.t option = "relatedTarget"
  [@@mel.get] [@@mel.return nullable]

  external screenX : t -> int = "screenX" [@@mel.get]
  external screenY : t -> int = "screenY" [@@mel.get]
  external shiftKey : t -> bool = "shiftKey" [@@mel.get]
end

type drag

module Drag = struct
  type t = drag event

  external altKey : t -> bool = "altKey" [@@mel.get]
  external button : t -> int = "button" [@@mel.get]
  external buttons : t -> int = "buttons" [@@mel.get]
  external clientX : t -> int = "clientX" [@@mel.get]
  external clientY : t -> int = "clientY" [@@mel.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@mel.get]
  external getModifierState : t -> bool = "getModifierState" [@@mel.send]
  external metaKey : t -> bool = "metaKey" [@@mel.get]
  external movementX : t -> int = "movementX" [@@mel.get]
  external movementY : t -> int = "movementY" [@@mel.get]
  external pageX : t -> int = "pageX" [@@mel.get]
  external pageY : t -> int = "pageY" [@@mel.get]

  external relatedTarget : t -> Target.t option = "relatedTarget"
  [@@mel.get] [@@mel.return nullable]

  external screenX : t -> int = "screenX" [@@mel.get]
  external screenY : t -> int = "screenY" [@@mel.get]
  external shiftKey : t -> bool = "shiftKey" [@@mel.get]
  external dataTransfer : t -> _ Js.t = "dataTransfer" [@@mel.get]
end

type pointer

module Pointer = struct
  type t = pointer event

  external detail : t -> int = "detail" [@@mel.get]
  external view : t -> Dom.window = "view" [@@mel.get]
  external screenX : t -> int = "screenX" [@@mel.get]
  external screenY : t -> int = "screenY" [@@mel.get]
  external clientX : t -> int = "clientX" [@@mel.get]
  external clientY : t -> int = "clientY" [@@mel.get]
  external pageX : t -> int = "pageX" [@@mel.get]
  external pageY : t -> int = "pageY" [@@mel.get]
  external movementX : t -> int = "movementX" [@@mel.get]
  external movementY : t -> int = "movementY" [@@mel.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@mel.get]
  external shiftKey : t -> bool = "shiftKey" [@@mel.get]
  external altKey : t -> bool = "altKey" [@@mel.get]
  external metaKey : t -> bool = "metaKey" [@@mel.get]
  external getModifierState : t -> bool = "getModifierState" [@@mel.send]
  external button : t -> int = "button" [@@mel.get]
  external buttons : t -> int = "buttons" [@@mel.get]

  external relatedTarget : t -> Target.t option = "relatedTarget"
  [@@mel.get] [@@mel.return nullable]

  external pointerId : t -> Dom.eventPointerId = "pointerId" [@@mel.get]
  external width : t -> float = "width" [@@mel.get]
  external height : t -> float = "height" [@@mel.get]
  external pressure : t -> float = "pressure" [@@mel.get]

  external tangentialPressure : t -> float = "tangentialPressure"
  [@@mel.get]

  external tiltX : t -> int = "tiltX" [@@mel.get]
  external tiltY : t -> int = "tiltY" [@@mel.get]
  external twist : t -> int = "twist" [@@mel.get]
  external pointerType : t -> string = "pointerType" [@@mel.get]
  external isPrimary : t -> bool = "isPrimary" [@@mel.get]
end

type touch

module Touch = struct
  type t = touch event

  external altKey : t -> bool = "altKey" [@@mel.get]
  external changedTouches : t -> _ Js.t = "changedTouches" [@@mel.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@mel.get]
  external getModifierState : t -> bool = "getModifierState" [@@mel.send]
  external metaKey : t -> bool = "metaKey" [@@mel.get]
  external shiftKey : t -> bool = "shiftKey" [@@mel.get]
  external targetTouches : t -> _ Js.t = "targetTouches" [@@mel.get]
  external touches : t -> _ Js.t = "touches" [@@mel.get]
end

type ui

module UI = struct
  type t = ui event

  external detail : t -> int = "detail" [@@mel.get]
  external view : t -> Dom.window = "view" [@@mel.get]
end

type wheel

module Wheel = struct
  type t = wheel event

  external deltaMode : t -> int = "deltaMode" [@@mel.get]
  external deltaX : t -> float = "deltaX" [@@mel.get]
  external deltaY : t -> float = "deltaY" [@@mel.get]
  external deltaZ : t -> float = "deltaZ" [@@mel.get]
end

type animation

module Animation = struct
  type t = animation event

  external animationName : t -> string = "animationName" [@@mel.get]
  external pseudoElement : t -> string = "pseudoElement" [@@mel.get]
  external elapsedTime : t -> float = "elapsedTime" [@@mel.get]
end

type transition

module Transition = struct
  type t = transition event

  external propertyName : t -> string = "propertyName" [@@mel.get]
  external pseudoElement : t -> string = "pseudoElement" [@@mel.get]
  external elapsedTime : t -> float = "elapsedTime" [@@mel.get]
end

type media

module Media = struct
  type t = media event
end

type image

module Image = struct
  type t = image event
end

type selection

module Selection = struct
  type t = selection event
end

type form

module Form = struct
  type t = form event
end
