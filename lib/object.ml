type t =
  | LoxNumber of float
  | LoxString of string
  | LoxBoolean of bool
  | LoxNil

let pp_obj obj =
  match obj with
  | LoxNumber x -> Printf.sprintf "%g" x
  | LoxString s -> Printf.sprintf "\"%s\"" s
  | LoxBoolean b -> string_of_bool b
  | LoxNil -> "nil"
