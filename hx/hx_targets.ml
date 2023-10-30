open Realm

type target =
  | Target_form_element of Webapi.Dom.HtmlFormElement.t
  | Target_element

type set = (React.element Promise.t -> React.element Promise.t) -> unit
type t = { set : set; target : target }

let targets : t Js.Dict.t = Js.Dict.empty ()
let set id me = Js.Dict.set targets id me
let get id = Js.Dict.get targets id
let unset id = (Js.Dict.unsafeDeleteKey (Obj.magic targets) id [@u])
