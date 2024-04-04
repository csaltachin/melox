type eof_error = Eof
type t = { tokens : Token.t list; previous : Token.t option }

let init tokens = { tokens; previous = None }

let peek parser =
  match parser.tokens with curr :: _ -> Ok curr | [] -> Error Eof

let peek_next parser =
  match parser.tokens with _ :: next :: _ -> Ok next | _ -> Error Eof

let peek_previous parser = parser.previous

let advance parser =
  match parser.tokens with
  | curr :: tail -> Ok { tokens = tail; previous = Some curr }
  | [] -> Error Eof

let advance_ceil parser = Result.value (advance parser) ~default:parser
