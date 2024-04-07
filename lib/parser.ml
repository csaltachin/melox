type ast = Ast.expression
(** Type of an AST node. For now, we only have expression nodes. *)

type t = { tokens : Token.t list; previous : Token.t option }
(** Type of parser. Maintains a list of (wrapped) tokens yet to be consumed, as
    well as the most recently consumed token.

    The functions in the [Parser] module assume that the token list does not end
    with an EOF token, e.g. as generated by [Scanner.scan ~drop_eof:true]. *)

type eof_error = Eof
type parse_error = UnexpectedToken | UnexpectedEof

type parse_result = (t * ast, parse_error) result
(** Result type for parse-consumers. *)

type parse_consumer = t -> parse_result
(** Type of "parse-consumers": operations which take in a parser state and emit
    either a pair (advanced parser state, new AST node) or a parse error. *)

(** Initialize a parser from a list of tokens. The functions in the [Parser]
    module assume that this list does not contain a trailing EOF token, e.g. as
    generated by [Scanner.scan ~drop_eof:true]. *)
let init tokens = { tokens; previous = None }

(** Peek the current, next-to-be-consumed token, i.e. the first element of
    [parser.tokens]. Returns [Error Eof] if the token list is empty. *)
let peek parser =
  match parser.tokens with curr :: _ -> Ok curr | [] -> Error Eof

(** Peek the token immediately after the current token, i.e. the second element
    of [parser.tokens]. Returns [Error Eof] if the token list has less than two
    elements. *)
let peek_next parser =
  match parser.tokens with _ :: next :: _ -> Ok next | _ -> Error Eof

(** Peek the most recently consumed token. Note that this will not be the first
    element of [parser.tokens], but rather the element immediately before it
    (before it was popped from the list). *)
let peek_previous parser = parser.previous

(** Consume one token, and return the new parser state (after advancing by one
    token). Returns [Error Eof] if there were no remaining tokens. *)
let advance parser =
  match parser.tokens with
  | curr :: tail -> Ok { tokens = tail; previous = Some curr }
  | [] -> Error Eof

(** Try to consume a token and return the new parser state. If there were no
    more tokens, return the same parser state.

    This is the same as [advance parser], but mapping [Ok parser'] to [parser']
    and [Error Eof] to [parser]. *)
let advance_ceil parser = Result.value (advance parser) ~default:parser

(** Peek the current token. If the peek succeeds as [Ok token] and
    [matches token.raw] is true, then consume the token, and return the advanced
    parser and [Some token]. Otherwise, return back the same parser state and
    [None]. *)
let advance_if_match (matches : Token.raw_t -> bool) parser =
  match peek parser with
  | Ok token when matches token.raw -> (advance_ceil parser, Some token)
  | _ -> (parser, None)

(** Same as [advance_if_match], but wraps the returned pair in [Ok], and returns
    [Error Eof] if peeking fails. *)
let advance_if_match_result (matches : Token.raw_t -> bool) parser =
  match peek parser with
  | Ok token when matches token.raw -> Ok (advance_ceil parser, Some token)
  | Ok _ -> Ok (parser, None)
  | Error Eof -> Error Eof

(** Given a parse-consumer [consumer] and a [matches] function, returns a new
    parse-consumer that (greedily) produces a left-associative, binary AST node,
    whose "leaves" are nodes produced by applications of [consumer]. A token
    [token] is considered an operator iff [matches (token.raw)] evaluates to
    true, and all such tokens are considered with equal precedence.

    In other words, let [pi] be a precedence level, and suppose [token] is an
    operator with precedence [pi] iff [matches (token.raw)] is true. Then
    [consume_binary_left_assoc consumer matches] is a parse-consumer that, given
    a parser state [parser], performs the following:

    (i) Attempt to apply [consumer] on [parser] to produce a new AST node [acc]
    and a parser state [parser'].

    (ii) Peek the current token [op] (if any). If it is an operator token with
    precedence [pi], then
    - consume [op];
    - attempt to apply [consumer] on the resulting parser state, producing an
      AST node [new_node] and a parser state [parser'];
    - replace [acc] with a new binary AST node, with operator token [op], right
      child [new_node], and left child [acc].

    (iii) Return the final AST node [acc] wrapped in [Ok], or forward any parse
    error encountered in any application of [consumer] in steps (i) and (ii).

    In particular, if [consumer] is tail-recursive, then this parse-consumer
    will also be tail-recursive. *)
let consume_binary_left_assoc (consumer : parse_consumer)
    (matches : Token.raw_t -> bool) : parse_consumer =
  let ( let* ) = Result.bind in
  let rec make_aux_list acc parser =
    match advance_if_match_result matches parser with
    | Error Eof -> Error UnexpectedEof
    | Ok (parser, None) -> Ok (parser, List.rev acc)
    | Ok (after_match, Some op_token) ->
        let* after_node, right_node = consumer after_match in
        let acc' = (op_token, right_node) :: acc in
        make_aux_list acc' after_node
  in
  let tree_of_aux_list first_node aux_list =
    match aux_list with
    | [] -> first_node
    | (first_op, second_node) :: rest ->
        let init =
          Ast.Binary { left = first_node; op = first_op; right = second_node }
        in
        let f acc aux_item =
          let op, right = aux_item in
          Ast.Binary { left = acc; op; right }
        in
        List.fold_left f init rest
  in
  fun parser ->
    let* after_first, first_node = consumer parser in
    let* after_last, aux_list = make_aux_list [] after_first in
    Ok (after_last, tree_of_aux_list first_node aux_list)

(* The following are parse-consumers for the Lox grammar rules, following the
   book. *)

let rec consume_expression : parse_consumer =
 fun parser -> consume_equality parser

and consume_equality : parse_consumer =
  let open Token in
  let matches raw_token =
    match raw_token with EqualEqual | BangEqual -> true | _ -> false
  in
  fun parser -> consume_binary_left_assoc consume_comparison matches parser

and consume_comparison : parse_consumer =
  let open Token in
  let matches raw_token =
    match raw_token with
    | Greater | GreaterEqual | Less | LessEqual -> true
    | _ -> false
  in
  fun parser -> consume_binary_left_assoc consume_term matches parser

and consume_term : parse_consumer =
  let open Token in
  let matches raw_token =
    match raw_token with Plus | Minus -> true | _ -> false
  in
  fun parser -> consume_binary_left_assoc consume_factor matches parser

and consume_factor : parse_consumer =
  let open Token in
  let matches raw_token =
    match raw_token with Star | Slash -> true | _ -> false
  in
  fun parser -> consume_binary_left_assoc consume_unary matches parser

and consume_unary : parse_consumer =
  let open Token in
  let ( let* ) = Result.bind in
  let is_unary_op raw_token =
    match raw_token with Bang | Minus -> true | _ -> false
  in
  fun parser ->
    match peek parser with
    | Error Eof -> Error UnexpectedEof
    | Ok op when is_unary_op op.raw ->
        let* after_node, right = consume_unary parser in
        let node = Ast.Unary { op; right } in
        Ok (after_node, node)
    | _ -> consume_primary parser

and consume_primary : parse_consumer =
  let open Token in
  let ( let* ) = Result.bind in
  fun parser ->
    let* peeked = peek parser |> Result.map_error (fun Eof -> UnexpectedEof) in
    match peeked.raw with
    | True ->
        let literal = Ast.Literal (Object.LoxBoolean true) in
        Ok (advance_ceil parser, literal)
    | False ->
        let literal = Ast.Literal (Object.LoxBoolean false) in
        Ok (advance_ceil parser, literal)
    | Nil ->
        let literal = Ast.Literal Object.LoxNil in
        Ok (advance_ceil parser, literal)
    | Number x ->
        let literal = Ast.Literal (Object.LoxNumber x) in
        Ok (advance_ceil parser, literal)
    | String s ->
        let literal = Ast.Literal (Object.LoxString s) in
        Ok (advance_ceil parser, literal)
    | LeftParen ->
        let after_left_paren = advance_ceil parser in
        let* after_inner, inner = consume_expression after_left_paren in
        let* peeked_right =
          peek parser |> Result.map_error (fun Eof -> UnexpectedEof)
        in
        if peeked_right.raw = RightParen then
          Ok (advance_ceil after_inner, Ast.Grouping inner)
        else Error UnexpectedToken
    | _ -> Error UnexpectedToken
