let rec evaluate_expression (expr : Ast.expression) : Object.t =
  match expr with
  | Literal value -> value
  | Grouping inner -> evaluate_expression inner
  | Unary { op; right } -> evaluate_unary op right
  | Binary { left; op; right } -> evaluate_binary op left right

and evaluate_unary (op : Token.t) (right : Ast.expression) : Object.t =
  let open Object in
  let open Token in
  let obj = evaluate_expression right in
  match (op.raw, obj) with
  (* Boolean negation: expects one argument of any type. Evaluates to true if
     its argument is truthy, false otherwise. *)
  | Bang, _ -> LoxBoolean (is_truthy obj)
  (* Number negation: expects one argument of type number. Evaluates to the
     float negation of the argument. *)
  | Minus, LoxNumber x -> LoxNumber (Float.neg x)
  | Minus, _ -> failwith "type error"
  (* If this case is reached, it means we parsed a bad operator token in this
     unary expression. *)
  | _ -> failwith "unreachable"

and evaluate_binary (op : Token.t) (left : Ast.expression)
    (right : Ast.expression) : Object.t =
  let open Object in
  let open Token in
  let left_obj = evaluate_expression left in
  let right_obj = evaluate_expression right in
  match (op.raw, left_obj, right_obj) with
  (* Equality operators *)
  (* - Equality: expects two arguments of any type. Evaluates to true if the
     arguments have equal types and values, false otherwise. *)
  | EqualEqual, _, _ -> LoxBoolean (is_equal left_obj right_obj)
  (* - Equality negation: expects two arguments of any type. Evaluates to false
     if the arguments have equal types and values, true otherwise. *)
  | BangEqual, _, _ -> LoxBoolean (is_equal left_obj right_obj |> not)
  (* Comparison operators *)
  (* - Greater than: expects two arguments of type number. Evaluates to true if
     the left value is greater than the right value, false otherwise. *)
  | Greater, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs > rhs)
  | Greater, _, _ -> failwith "type error"
  (* - Greater than or equal to: expects two arguments of type number. Evaluates
     to true if the left value is greater than or equal to the right value,
     false otherwise. *)
  | GreaterEqual, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs >= rhs)
  | GreaterEqual, _, _ -> failwith "type error"
  (* - Less than: expects two arguments of type number. Evaluates to true if the
     left value is less than the right value, false otherwise. *)
  | Less, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs < rhs)
  | Less, _, _ -> failwith "type error"
  (* - Less than or equal to: expects two arguments of type number. Evaluates to
     true if the left value is less than or equal to the right value, false
     otherwise. *)
  | LessEqual, LoxNumber lhs, LoxNumber rhs -> LoxBoolean (lhs <= rhs)
  | LessEqual, _, _ -> failwith "type error"
  (* Arithmetic operators *)
  (* - Subtraction: expects two arguments of type number. Evaluates to the left
     value minus the right value. *)
  | Minus, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs -. rhs)
  | Minus, _, _ -> failwith "type error"
  (* - Multiplication: expects two arguments of type number. Evaluates to the
     product of the two values. *)
  | Star, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs *. rhs)
  | Star, _, _ -> failwith "type error"
  (* - Division: expects two arguments of type number. Evaluates to the left
     value divided by the right value. *)
  | Slash, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs /. rhs)
  | Slash, _, _ -> failwith "type error"
  (* Plus (two overloads) *)
  (* - Addition: expects two arguments of type number. Evaluates to the sum of
     the two values. *)
  | Plus, LoxNumber lhs, LoxNumber rhs -> LoxNumber (lhs +. rhs)
  (* - String concatenation: expects two arguments of type string. Evaluates to
     the concatenation of the two strings. *)
  | Plus, LoxString left_str, LoxString right_str ->
      LoxString (left_str ^ right_str)
  | Plus, _, _ -> failwith "type error"
  (* If this case is reached, it means we parsed a bad operator token in this
     binary expression. *)
  | _ -> failwith "unreachable"
