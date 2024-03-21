type token = Token.t
type obj = Object.t

type expression =
  | Binary of { left : expression; op : token; right : expression }
  | Unary of { op : token; right : expression }
  | Literal of obj
  | Grouping of expression
