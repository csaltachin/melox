type ast = Ast.expression
type t = { tokens : Token.t list; previous : Token.t option }
type eof_error = Eof
type parse_error = UnexpectedToken | UnexpectedEof
type parse_result = (t * ast, parse_error) result
type parse_consumer = t -> parse_result

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

let advance_if_match (matches : Token.raw_t -> bool) parser =
  match peek parser with
  | Ok token when matches token.raw -> (advance_ceil parser, Some token)
  | _ -> (advance_ceil parser, None)

let consume_binary_left_assoc (consumer : parse_consumer)
    (matches : Token.raw_t -> bool) : parse_consumer =
  let ( let* ) = Result.bind in
  let rec make_aux_list acc parser =
    match advance_if_match matches parser with
    | parser, None -> Ok (parser, List.rev acc)
    | after_match, Some op_token ->
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
