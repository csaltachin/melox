type t =
  | LoxNumber of float
  | LoxString of string
  | LoxBoolean of bool
  | LoxNil

let is_truthy obj =
  match obj with LoxBoolean false | LoxNil -> false | _ -> true

let is_equal left_obj right_obj =
  match (left_obj, right_obj) with
  | LoxNil, LoxNil -> true
  | LoxBoolean left_bool, LoxBoolean right_bool -> left_bool = right_bool
  | LoxString left_str, LoxString right_str -> left_str = right_str
  | LoxNumber left_num, LoxNumber right_num -> left_num = right_num
  | _, _ -> false

let pp_obj obj =
  match obj with
  | LoxNumber x -> Printf.sprintf "%g" x
  | LoxString s -> Printf.sprintf "\"%s\"" s
  | LoxBoolean b -> string_of_bool b
  | LoxNil -> "nil"
