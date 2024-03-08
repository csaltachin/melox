(** Type of scanner. Maintains current position, column and line number. *)
type t =
  { source : string
  ; len : int
  ; pos : int
  ; col : int
  ; line : int
  }

type scanned_t =
  | RawToken of Token.raw_t
  | Whitespace
  | Comment

type eof_error = Eof

type scanner_error =
  | UnterminatedStringLiteral
  | UnterminatedBlockComment
  | UnrecognizedCharacter

let init source =
  let len = String.length source in
  { source; len; pos = 0; col = 0; line = 0 }

let is_at_end scanner = scanner.pos = scanner.len

(** Peek the character at the current position. Return Error Eof if the scanner was already at the end of file. *)
let peek scanner =
  if is_at_end scanner then Error Eof else Ok scanner.source.[scanner.pos]

(** Peek the character at the position right after the current one. Return Error Eof if said position is past the end of file. *)
let peek_next scanner =
  let can_peek = scanner.pos < scanner.len - 1 in
  if can_peek then Ok scanner.source.[scanner.pos + 1] else Error Eof

(** Advance the scanner by 1 character, updating line and column numbers. Returns Error Eof if the scanner was already at the end of file. *)
let advance scanner =
  match peek scanner with
  | Ok '\n' -> Ok { scanner with pos = scanner.pos + 1; col = 0; line = scanner.line + 1 }
  | Ok _ -> Ok { scanner with pos = scanner.pos + 1; col = scanner.col + 1 }
  | Error Eof -> Error Eof

(** Advance the scanner by 1 character and return it directly. If the scanner was already at the end of file, simply returns the original scanner. *)
let advance_ceil scanner =
  match advance scanner with
  | Ok advanced -> advanced
  | Error Eof -> scanner

(** Advance the scanner until we either peek a char c such that (f c) evaluates to true, or we reach the end of file. Return the advanced scanner and the consumed substring. *)
let consume_until scanner f =
  let rec aux start_pos acc_len curr_scanner =
    match peek curr_scanner with
    | Ok char when f char ->
      curr_scanner, String.sub curr_scanner.source start_pos acc_len
    | Error Eof -> curr_scanner, String.sub curr_scanner.source start_pos acc_len
    | Ok _ -> aux start_pos (acc_len + 1) (advance_ceil scanner)
  in
  aux scanner.pos 0 scanner

(** Same as consume_until, but does not return the consumed substring; just the advanced scanner. *)
let ignore_until scanner f =
  let rec aux curr_scanner =
    match peek curr_scanner with
    | Ok char when f char -> curr_scanner
    | Error Eof -> curr_scanner
    | Ok _ -> aux (advance_ceil scanner)
  in
  aux scanner

let is_digit char =
  match char with
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_or_underscore char =
  match char with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(** Consume a string literal lexeme until either a closing '"' or end of file is reached. Assumes the opening '"' has already been consumed; consumes the closing '"' if found. The returned boolean is true iff the closing '"' was reached and consumed. *)
let consume_string_literal scanner =
  let f char = char = '"' in
  consume_until scanner f

(** Consume an identifier lexeme. Yields the lexeme as a string. *)
let consume_identifier_literal scanner =
  let f char = not (is_alpha_or_underscore char || is_digit char) in
  consume_until scanner f

(** Consume a number literal lexeme. Converts the lexeme into a float value before returning. *)
let consume_number_literal scanner =
  let f char = not (is_digit char) in
  let scanner_after_int_part, int_part = consume_until scanner f in
  let scanner_advanced, lexeme =
    match peek scanner_after_int_part, peek_next scanner_after_int_part with
    | Ok '.', Ok char when is_digit char ->
      consume_until (advance_ceil scanner_after_int_part) f
    | _ -> scanner_after_int_part, int_part
  in
  scanner_advanced, float_of_string lexeme

(** Advances the scanner until either a newline or end of file is reached, ignoring characters in the way. Consumes the newline if found. *)
let consume_line_comment scanner =
  let f char = char = '\n' in
  let scanner_at_newline_or_eof = ignore_until scanner f in
  advance_ceil scanner_at_newline_or_eof

(** Advances the scanner until either "*/" or end of file is reached, ignoring other characters in the way. Assumes the opening "/*" has already been consumed; consumes the final "*/" if found. The returned boolean is true iff the closing "*/" was reached and consumed. *)
let consume_block_comment scanner =
  let rec aux curr_scanner =
    match peek curr_scanner, peek_next curr_scanner with
    | Ok '*', Ok '/' -> curr_scanner |> advance_ceil |> advance_ceil, true
    | Error Eof, _ -> curr_scanner, false
    | _ -> aux (advance_ceil curr_scanner)
  in
  aux scanner

let consume_lexeme scanner =
  let open Token in
  match peek scanner with
  (* Unambiguous single-char tokens *)
  | Ok '(' -> advance_ceil scanner, Ok (RawToken LeftParen)
  | Ok ')' -> advance_ceil scanner, Ok (RawToken RightParen)
  | Ok '[' -> advance_ceil scanner, Ok (RawToken LeftBrace)
  | Ok ']' -> advance_ceil scanner, Ok (RawToken RightBrace)
  | Ok ',' -> advance_ceil scanner, Ok (RawToken Comma)
  | Ok '.' -> advance_ceil scanner, Ok (RawToken Dot)
  | Ok ';' -> advance_ceil scanner, Ok (RawToken Semicolon)
  | Ok '+' -> advance_ceil scanner, Ok (RawToken Plus)
  | Ok '-' -> advance_ceil scanner, Ok (RawToken Minus)
  | Ok '*' -> advance_ceil scanner, Ok (RawToken Star)
  (* Tokens that can be one or two chars, need peeking *)
  | Ok '!' ->
    (match peek_next scanner with
     | Ok '=' -> scanner |> advance_ceil |> advance_ceil, Ok (RawToken BangEqual)
     | _ -> advance_ceil scanner, Ok (RawToken Bang))
  | Ok '=' ->
    (match peek_next scanner with
     | Ok '=' -> scanner |> advance_ceil |> advance_ceil, Ok (RawToken EqualEqual)
     | _ -> advance_ceil scanner, Ok (RawToken Equal))
  | Ok '>' ->
    (match peek_next scanner with
     | Ok '=' -> scanner |> advance_ceil |> advance_ceil, Ok (RawToken GreaterEqual)
     | _ -> advance_ceil scanner, Ok (RawToken Greater))
  | Ok '<' ->
    (match peek_next scanner with
     | Ok '=' -> scanner |> advance_ceil |> advance_ceil, Ok (RawToken LessEqual)
     | _ -> advance_ceil scanner, Ok (RawToken Less))
  (* Slash and comments *)
  | Ok '/' ->
    (match peek_next scanner with
     | Ok '/' -> consume_line_comment scanner, Ok Comment
     | Ok '*' ->
       let advanced, comment_terminated =
         scanner |> advance_ceil |> advance_ceil |> consume_block_comment
       in
       advanced, if comment_terminated then Ok Comment else Error UnterminatedBlockComment
     | _ -> advance_ceil scanner, Ok (RawToken Slash))
  (* Unrecognized characters *)
  | _ -> scanner, Error UnrecognizedCharacter