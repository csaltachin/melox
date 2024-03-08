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

(** Type of tokens produced by the scanner. Wraps a raw token with line and column information for nicer error reporting. *)
type t =
  { raw : raw_t
  ; start_line : int
  ; start_col : int
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
  Printf.sprintf "%s; starts at line %i, col %i" of_raw token.start_line token.start_col

let wrap_token raw_token start_line start_col : t =
  { raw = raw_token; start_line; start_col }
