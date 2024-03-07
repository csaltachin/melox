(** Type of scanner. Maintains current position, column and line number. *)
type t =
  { source : string
  ; len : int
  ; pos : int
  ; col : int
  ; line : int
  }

type eof_error = EofError

let init source =
  let len = String.length source in
  { source; len; pos = 0; col = 0; line = 0 }

let is_at_end scanner = scanner.pos = scanner.len

(** Peek the character at the current position. Return Error EofError if the scanner was already at the end of file. *)
let peek scanner =
  if is_at_end scanner then Error EofError else Ok scanner.source.[scanner.pos]

(** Peek the character at the position right after the current one. Return Error EofError if said position is past the end of file. *)
let peek_next scanner =
  let can_peek = scanner.pos < scanner.len - 1 in
  if can_peek then Ok scanner.source.[scanner.pos + 1] else Error EofError

(** Advance the scanner by 1 character, updating line and column numbers. Returns Error EofError if the scanner was already at the end of file. *)
let advance scanner =
  match peek scanner with
  | Ok '\n' -> Ok { scanner with pos = scanner.pos + 1; col = 0; line = scanner.line + 1 }
  | Ok _ -> Ok { scanner with pos = scanner.pos + 1; col = scanner.col + 1 }
  | Error EofError -> Error EofError

(** Advance the scanner by 1 character and return it directly. If the scanner was already at the end of file, simply returns the original scanner. *)
let advance_ceil scanner =
  match advance scanner with
  | Ok advanced -> advanced
  | Error EofError -> scanner

(** Advance the scanner until we either peek a char c such that (f c) evaluates to true, or we reach the end of file. Return the advanced scanner and the consumed substring. *)
let consume_until scanner f =
  let rec aux start_pos acc_len curr_scanner =
    match peek curr_scanner with
    | Ok char when f char ->
      curr_scanner, String.sub curr_scanner.source start_pos acc_len
    | Error EofError -> curr_scanner, String.sub curr_scanner.source start_pos acc_len
    | Ok _ -> aux start_pos (acc_len + 1) (advance_ceil scanner)
  in
  aux scanner.pos 0 scanner

let is_digit char =
  match char with
  | '0' .. '9' -> true
  | _ -> false

let is_alpha_or_underscore char =
  match char with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(** Consume a string literal lexeme until a closing '"' is peeked (not consumed), or end of file is reached. Assumes the opening '"' has already been consumed. *)
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
