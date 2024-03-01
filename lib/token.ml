(* Type of raw tokens. The variants for literals wrap the lexeme as a string. *)
type raw_t =
  (* 1-char tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
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

(* Type of tokens produced by the scanner. Wrapps a raw token with line and offset information for nicer error reporting. *)
type t =
  { raw : raw_t
  ; line : int
  ; offset : int
  }
