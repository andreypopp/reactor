module To_json = struct
type user = int [@@deriving to_json]
type 'a param = 'a [@@deriving to_json]
type opt = string option [@@deriving to_json]
type tuple = int * string [@@deriving to_json]
type record = { name : string; age : int } [@@deriving to_json]
type sum = A | B of int | C of { name : string } [@@deriving to_json]
type other = [ `C ] [@@deriving to_json] type poly = [ `A | `B of int | other ] [@@deriving to_json]
type 'a c = [ `C of 'a ] [@@deriving to_json]
type recur = A | Fix of recur [@@deriving to_json]
type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving to_json]
end
module Of_json = struct
type user = int [@@deriving of_json]
type 'a param = 'a [@@deriving of_json]
type opt = string option [@@deriving of_json]
type tuple = int * string [@@deriving of_json]
type record = { name : string; age : int } [@@deriving of_json]
type sum = A | B of int | C of { name : string } [@@deriving of_json]
type other = [ `C ] [@@deriving of_json] type poly = [ `A | `B of int | other ] [@@deriving of_json]
type 'a c = [ `C of 'a ] [@@deriving of_json]
type recur = A | Fix of recur [@@deriving of_json]
type polyrecur = [ `A | `Fix of polyrecur ] [@@deriving of_json]
end
module Json = struct
type user = int [@@deriving json]
end
