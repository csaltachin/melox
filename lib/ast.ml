type token = Token.t
type obj = Object.t

type expression =
  | Binary of { left : expression; op : token; right : expression }
  | Unary of { op : token; right : expression }
  | Literal of obj
  | Grouping of expression

let rec pp expr =
  match expr with
  | Binary { left; op; right } ->
      Printf.sprintf "(%s %s %s)"
        (Token.string_of_raw op.raw)
        (pp left) (pp right)
  | Unary { op; right } ->
      Printf.sprintf "(%s %s)" (Token.string_of_raw op.raw) (pp right)
  | Literal value -> Object.pp_obj value
  | Grouping inner -> Printf.sprintf "(%s)" (pp inner)
