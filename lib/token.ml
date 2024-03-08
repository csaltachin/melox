(** Type of raw tokens. The variants for literals wrap the lexeme as a string. *)
type raw_t =
  (* 1-char tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Plus
  | Minus
  | Slash
  | Star
  | Semicolon
  (* 1-char and 2-char tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* Literals *)
  | Identifier of string
  | String of string
  | Number of float
  (* Reserved keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* End of file *)
  | EndOfFile

type line_range =
  | Single of int
  | Multi of int * int

(** Type of tokens produced by the scanner. Wraps a raw token with line and column information for nicer error reporting. *)
type t =
  { raw : raw_t
  ; lines : line_range
  ; start_col : int
  ; end_col : int
  }

let string_of_raw raw_token =
  match raw_token with
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Slash -> "Slash"
  | Star -> "Star"
  | Semicolon -> "Semicolon"
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | Identifier ident -> "Identifier |> " ^ ident
  | String literal -> "StringLiteral |> \"" ^ literal ^ "\""
  | Number number -> "NumberLiteral |> " ^ string_of_float number
  | And -> "And"
  | Class -> "Class"
  | Else -> "Else"
  | False -> "False"
  | Fun -> "Fun"
  | For -> "For"
  | If -> "If"
  | Nil -> "Nil"
  | Or -> "Or"
  | Print -> "Print"
  | Return -> "Return"
  | Super -> "Super"
  | This -> "This"
  | True -> "True"
  | Var -> "Var"
  | While -> "While"
  | EndOfFile -> "EndOfFile"

let string_of_wrapped token =
  let of_raw = string_of_raw token.raw in
  match token.lines with
  | Single line ->
    if token.start_col + 1 = token.end_col
    then Printf.sprintf "%s; at line %i, col %i" of_raw line token.start_col
    else
      (* TODO: EndOfFile tokens will always have start_col = end_col, so they will show as "col x - (x-1)". We could match for them and print them more nicely (maybe even add a Token.t variant for it or is that too much?). All other tokens always have nonempty lexemes, so that start_col < end_col, and so this branch is fine for them. *)
      Printf.sprintf
        "%s; at line %i, col %i - %i"
        of_raw
        line
        token.start_col
        (token.end_col - 1)
  | Multi (start_line, end_line) ->
    Printf.sprintf
      "%s; from (line %i, col %i) to (line %i, col %i)"
      of_raw
      start_line
      token.start_col
      end_line
      (token.end_col - 1)

let wrap_token raw_token line_pair col_pair : t =
  let start_line, end_line = line_pair in
  let start_col, end_col = col_pair in
  let lines =
    if start_line = end_line then Single start_line else Multi (start_line, end_line)
  in
  { raw = raw_token; lines; start_col; end_col }
