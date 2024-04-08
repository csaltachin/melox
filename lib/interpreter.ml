type runtime_error =
  | DivisionByZero
  | TypeError of { expected : string; actual : string }

type eval_result = (Object.t, runtime_error) result

let eval_ok (value : Object.t) = Ok value
let eval_error (error : runtime_error) = Error error

(* Expression evaluators *)

let rec evaluate_expression (expr : Ast.expression) : eval_result =
  match expr with
  | Literal value -> eval_ok value
  | Grouping inner -> evaluate_expression inner
  | Unary { op; right } -> evaluate_unary op right
  | Binary { left; op; right } -> evaluate_binary op left right

and evaluate_unary (op : Token.t) (right : Ast.expression) : eval_result =
  let ( let* ) = Result.bind in
  let open Object in
  let open Token in
  let* obj = evaluate_expression right in
  match (op.raw, obj) with
  (* Boolean negation: expects one argument of any type. Evaluates to true if
     its argument is truthy, false otherwise. *)
  | Bang, _ -> LoxBoolean (is_truthy obj) |> eval_ok
  (* Number negation: expects one argument of type number. Evaluates to the
     float negation of the argument. *)
  | Minus, LoxNumber x -> LoxNumber (Float.neg x) |> eval_ok
  | Minus, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* If this case is reached, it means we parsed a bad operator token in this
     unary expression. *)
  | actual_raw, _ ->
      let string_of_actual_raw = string_of_raw actual_raw in
      let msg =
        Printf.sprintf
          "Found unexpected operator token in unary expression; found %s, \
           expected one of [Bang; Minus]"
          string_of_actual_raw
      in
      failwith msg

and evaluate_binary (op : Token.t) (left : Ast.expression)
    (right : Ast.expression) : eval_result =
  let ( let* ) = Result.bind in
  let open Object in
  let open Token in
  let* left_obj = evaluate_expression left in
  let* right_obj = evaluate_expression right in
  match (op.raw, left_obj, right_obj) with
  (* Equality operators *)
  (* - Equality: expects two arguments of any type. Evaluates to true if the
     arguments have equal types and values, false otherwise. *)
  | EqualEqual, _, _ -> LoxBoolean (is_equal left_obj right_obj) |> eval_ok
  (* - Equality negation: expects two arguments of any type. Evaluates to false
     if the arguments have equal types and values, true otherwise. *)
  | BangEqual, _, _ ->
      LoxBoolean (is_equal left_obj right_obj |> not) |> eval_ok
  (* Comparison operators *)
  (* - Greater than: expects two arguments of type number. Evaluates to true if
     the left value is greater than the right value, false otherwise. *)
  | Greater, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs > rhs) |> eval_ok
  | Greater, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | Greater, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Greater than or equal to: expects two arguments of type number. Evaluates
     to true if the left value is greater than or equal to the right value,
     false otherwise. *)
  | GreaterEqual, LoxNumber lhs, LoxNumber rhs ->
      LoxBoolean (lhs >= rhs) |> eval_ok
  | GreaterEqual, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | GreaterEqual, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Less than: expects two arguments of type number. Evaluates to true if the
     left value is less than the right value, false otherwise. *)
  | Less, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs < rhs) |> eval_ok
  | Less, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | Less, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Less than or equal to: expects two arguments of type number. Evaluates to
     true if the left value is less than or equal to the right value, false
     otherwise. *)
  | LessEqual, LoxNumber lhs, LoxNumber rhs ->
      LoxBoolean (lhs <= rhs) |> eval_ok
  | LessEqual, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | LessEqual, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* Arithmetic operators *)
  (* - Subtraction: expects two arguments of type number. Evaluates to the left
     value minus the right value. *)
  | Minus, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs -. rhs) |> eval_ok
  | Minus, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | Minus, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Multiplication: expects two arguments of type number. Evaluates to the
     product of the two values. *)
  | Star, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs *. rhs) |> eval_ok
  | Star, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | Star, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Division: expects two arguments of type number. Evaluates to the left
     value divided by the right value. *)
  | Slash, LoxNumber _, LoxNumber 0.0 -> Error DivisionByZero
  | Slash, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs /. rhs) |> eval_ok
  | Slash, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  | Slash, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* Plus (two overloads) *)
  (* - Addition: expects two arguments of type number. Evaluates to the sum of
     the two values. *)
  | Plus, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs +. rhs) |> eval_ok
  | Plus, LoxNumber _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - String concatenation: expects two arguments of type string. Evaluates to
     the concatenation of the two strings. *)
  | Plus, LoxString left_str, LoxString right_str ->
      LoxString (left_str ^ right_str) |> eval_ok
  | Plus, LoxString _, bad_obj ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number"; actual } |> eval_error
  (* - Type error when the first argument does not match either overload. *)
  | Plus, bad_obj, _ ->
      let actual = string_of_type bad_obj in
      TypeError { expected = "number or string"; actual } |> eval_error
  (* If this case is reached, it means we parsed a bad operator token in this
     binary expression. *)
  | actual_raw, _, _ ->
      let string_of_actual_raw = string_of_raw actual_raw in
      let msg =
        Printf.sprintf
          "Found unexpected operator token in binary expression; found %s, \
           expected one of [EqualEqual; BangEqual; Greater; GreaterEqual; \
           Less; LessEqual; Plus; Minus; Star; Slash]"
          string_of_actual_raw
      in
      failwith msg
