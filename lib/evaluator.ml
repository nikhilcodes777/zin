open Ast
open Tokens
module StringMap = Map.Make (String)

(* NOTE: Map gets value with log(n) we can improve it by using hashtbl*)
type environment = {
  store : eval_object StringMap.t;
  outer : environment option;
}

and eval_object =
  | OBJ_INT of int
  | OBJ_BOOL of bool
  | OBJ_STRING of string
  | OBJ_BUILTIN of string
  | OBJ_NULL
  | OBJ_ERROR of string
  | OBJ_RETURN of eval_object
  | OBJ_FUNCTION of {
      parameters : identifier list;
      body : block;
      env : environment;
    }
  | OBJ_LIST of eval_object list

let rec describeObject = function
  | OBJ_INT i -> string_of_int i
  | OBJ_BOOL b -> string_of_bool b
  | OBJ_STRING s -> s
  | OBJ_BUILTIN _ -> "builtin_function"
  | OBJ_NULL -> "null"
  | OBJ_RETURN r -> describeObject r
  | OBJ_ERROR e -> "Error: " ^ e
  | OBJ_FUNCTION { parameters; _ } ->
      "fn(" ^ String.concat ", " parameters ^ ") { ... }"
  | OBJ_LIST elems ->
      "[" ^ (List.map describeObject elems |> String.concat ", ") ^ "]"

let getTypeName = function
  | OBJ_INT _ -> "integer"
  | OBJ_BOOL _ -> "boolean"
  | OBJ_STRING _ -> "string"
  | OBJ_NULL -> "null"
  | OBJ_BUILTIN _ -> "builtin"
  | OBJ_ERROR _ -> "error"
  | OBJ_RETURN _ -> "return value"
  | OBJ_FUNCTION _ -> "function"
  | OBJ_LIST _ -> "list"

let isBuiltin name =
  match name with
  | "len" | "print" | "println" | "str" | "int" | "type" | "head" | "tail"
  | "last" ->
      true
  | _ -> false

let callBuiltin name args =
  match (name, args) with
  | "len", [ OBJ_STRING s ] -> OBJ_INT (String.length s)
  | "len", [ OBJ_LIST l ] -> OBJ_INT (List.length l)
  | "len", [ arg ] ->
      OBJ_ERROR ("len() expects a string or a list, got " ^ getTypeName arg)
  | "len", args ->
      OBJ_ERROR
        ("len() expects 1 argument, got " ^ string_of_int (List.length args))
  | "print", [ arg ] ->
      print_string (describeObject arg);
      flush_all ();
      OBJ_NULL
  | "print", args ->
      OBJ_ERROR
        ("print() expects 1 argument, got " ^ string_of_int (List.length args))
  | "println", [ arg ] ->
      print_endline (describeObject arg);
      flush_all ();
      OBJ_NULL
  | "println", args ->
      OBJ_ERROR
        ("println() expects 1 argument, got " ^ string_of_int (List.length args))
  | "str", [ arg ] -> OBJ_STRING (describeObject arg)
  | "str", args ->
      OBJ_ERROR
        ("str() expects 1 argument, got " ^ string_of_int (List.length args))
  | "int", [ OBJ_STRING s ] -> (
      try OBJ_INT (int_of_string s)
      with Failure _ -> OBJ_ERROR ("Cannot convert '" ^ s ^ "' to integer"))
  | "int", [ OBJ_INT i ] -> OBJ_INT i
  | "int", [ arg ] ->
      OBJ_ERROR ("int() expects a string or integer, got " ^ getTypeName arg)
  | "int", args ->
      OBJ_ERROR
        ("int() expects 1 argument, got " ^ string_of_int (List.length args))
  (* type: returns type name of value *)
  | "type", [ arg ] -> OBJ_STRING (getTypeName arg)
  | "type", args ->
      OBJ_ERROR
        ("type() expects 1 argument, got " ^ string_of_int (List.length args))
  | "head", [ OBJ_LIST [] ] -> OBJ_NULL
  | "head", [ OBJ_LIST (h :: _) ] -> h
  | "head", [ arg ] ->
      OBJ_ERROR ("first() expects a list, got " ^ getTypeName arg)
  | "head", args ->
      OBJ_ERROR
        ("first() expects 1 argument, got " ^ string_of_int (List.length args))
  | "last", [ OBJ_LIST [] ] -> OBJ_NULL
  | "last", [ OBJ_LIST l ] -> OBJ_LIST [ List.hd (List.rev l) ]
  | "last", [ arg ] ->
      OBJ_ERROR ("last() expects a list, got " ^ getTypeName arg)
  | "last", args ->
      OBJ_ERROR
        ("last() expects 1 argument, got " ^ string_of_int (List.length args))
  | "tail", [ OBJ_LIST [] ] -> OBJ_NULL
  | "tail", [ OBJ_LIST (_ :: t) ] -> OBJ_LIST t
  | "tail", [ arg ] ->
      OBJ_ERROR ("rest() expects a list, got " ^ getTypeName arg)
  | "tail", args ->
      OBJ_ERROR
        ("rest() expects 1 argument, got " ^ string_of_int (List.length args))
  | name, _ -> OBJ_ERROR ("Unknown builtin function: " ^ name)

let empty_env = { store = StringMap.empty; outer = None }
let newEnclosedEnv outer = { store = StringMap.empty; outer = Some outer }

let rec getVar env name =
  try Some (StringMap.find name env.store)
  with Not_found -> (
    match env.outer with Some outer -> getVar outer name | None -> None)

let setVar env name value =
  { env with store = StringMap.add name value env.store }

let bindParameters outer params args =
  let enclosed_env = newEnclosedEnv outer in
  try
    let param_arg_pairs = List.combine params args in
    List.fold_left
      (fun env (param, arg) -> setVar env param arg)
      enclosed_env param_arg_pairs
  with Invalid_argument _ -> enclosed_env (* Lists have different lengths *)

let rec eval node env =
  match node with
  | Program program -> evalStatements program env
  | Expression expr -> evalExpression expr env
  | Statement stmt -> evalStatement stmt env

and isTruthy = function
  | OBJ_BOOL true -> true
  | OBJ_BOOL false -> false
  | OBJ_NULL -> false
  | _ -> true

and evalPrefixExpression op r =
  match (op, r) with
  | BANG, r -> OBJ_BOOL (r |> isTruthy |> not)
  | MINUS, OBJ_INT i -> OBJ_INT (-1 * i)
  | MINUS, other ->
      OBJ_ERROR
        ("Cannot apply '-' to " ^ getTypeName other
       ^ ". Expected an integer, got: " ^ describeObject other)
  | _ ->
      OBJ_ERROR
        ("Unknown prefix operator '" ^ describeToken op ^ "' applied to "
       ^ getTypeName r)

and evalInfixExpression left operator right =
  match (left, operator, right) with
  | OBJ_INT l, PLUS, OBJ_INT r -> OBJ_INT (l + r)
  | OBJ_STRING l, PLUS, OBJ_STRING r -> OBJ_STRING (l ^ r)
  | OBJ_LIST l, PLUS, OBJ_LIST r -> OBJ_LIST (l @ r)
  | element, PLUS, OBJ_LIST l -> OBJ_LIST (element :: l)
  | OBJ_LIST l, PLUS, element -> OBJ_LIST (l @ [ element ])
  | OBJ_INT l, MINUS, OBJ_INT r -> OBJ_INT (l - r)
  | OBJ_INT l, ASTERISK, OBJ_INT r -> OBJ_INT (l * r)
  | OBJ_INT l, SLASH, OBJ_INT r ->
      if r = 0 then
        OBJ_ERROR
          ("Division by zero: cannot divide " ^ string_of_int l ^ " by 0")
      else OBJ_INT (l / r)
  | OBJ_INT l, LT, OBJ_INT r -> OBJ_BOOL (l < r)
  | OBJ_INT l, GT, OBJ_INT r -> OBJ_BOOL (l > r)
  | OBJ_INT l, LT_EQUAL, OBJ_INT r -> OBJ_BOOL (l <= r)
  | OBJ_INT l, GT_EQUAL, OBJ_INT r -> OBJ_BOOL (l >= r)
  | l, EQUAL, r -> OBJ_BOOL (l = r)
  | l, NOT_EQUAL, r -> OBJ_BOOL (l <> r)
  | ( left_val,
      (PLUS | MINUS | ASTERISK | SLASH | LT | GT | LT_EQUAL | GT_EQUAL),
      right_val ) ->
      let op_name =
        match operator with
        | PLUS -> "addition (+)"
        | MINUS -> "subtraction (-)"
        | ASTERISK -> "multiplication (*)"
        | SLASH -> "division (/)"
        | LT -> "less than (<)"
        | GT -> "greater than (>)"
        | LT_EQUAL -> "less than or equal (<=)"
        | GT_EQUAL -> "greater than or equal (>=)"
        | _ -> "comparison"
      in
      OBJ_ERROR
        (Printf.sprintf
           "Type mismatch in %s: cannot operate on %s (%s) and %s (%s)" op_name
           (getTypeName left_val) (describeObject left_val)
           (getTypeName right_val) (describeObject right_val))
  | _ -> OBJ_ERROR ("unknown operator: " ^ describeToken operator)

and evalExpression expr env =
  match expr with
  | IntLiteral i -> (OBJ_INT i, env)
  | BoolLiteral b -> (OBJ_BOOL b, env)
  | StringLiteral s -> (OBJ_STRING s, env)
  | Ident name -> (
      if isBuiltin name then (OBJ_BUILTIN name, env)
      else
        match getVar env name with
        | Some value -> (value, env)
        | None -> (OBJ_ERROR ("variable not defined: " ^ name), env))
  | Prefix { operator; right } -> (
      let right_obj, env' = evalExpression right env in
      match right_obj with
      | OBJ_ERROR _ -> (right_obj, env')
      | _ -> (evalPrefixExpression operator right_obj, env))
  | Infix { left; operator; right } -> (
      let left_obj, env' = evalExpression left env in
      match left_obj with
      | OBJ_ERROR _ -> (left_obj, env')
      | _ -> (
          let right_obj, env'' = evalExpression right env' in
          match right_obj with
          | OBJ_ERROR _ -> (right_obj, env'')
          | _ -> (evalInfixExpression left_obj operator right_obj, env'')))
  | RangeExpression { starting; ending } -> (
      let start_obj, env' = evalExpression starting env in
      match start_obj with
      | OBJ_INT start_val -> (
          let end_obj, env'' = evalExpression ending env' in
          match end_obj with
          | OBJ_INT end_val ->
              if start_val <= end_val then
                let rec generate_range current_val acc =
                  if current_val > end_val then List.rev acc
                  else
                    generate_range (current_val + 1) (OBJ_INT current_val :: acc)
                in
                (OBJ_LIST (generate_range start_val []), env'')
              else
                ( OBJ_ERROR
                    "Range error: starting value must be less than or equal to \
                     ending value",
                  env'' )
          | _ ->
              ( OBJ_ERROR
                  ("Range error: ending value must be an integer, got "
                 ^ getTypeName end_obj),
                env' ))
      | _ ->
          ( OBJ_ERROR
              ("Range error: starting value must be an integer, got "
             ^ getTypeName start_obj),
            env' ))
  | IndexExpression { left; index } -> (
      let left_obj, env' = evalExpression left env in
      match left_obj with
      | OBJ_ERROR _ -> (left_obj, env')
      | OBJ_LIST elems -> (
          let index_obj, env'' = evalExpression index env' in
          match index_obj with
          | OBJ_INT i ->
              if i >= 0 && i < List.length elems then (List.nth elems i, env'')
              else
                ( OBJ_ERROR
                    (Printf.sprintf
                       "Index out of bounds: index %d for list of length %d" i
                       (List.length elems)),
                  env'' )
          | _ ->
              ( OBJ_ERROR
                  ("Index error: index must be an integer, got "
                 ^ getTypeName index_obj),
                env'' ))
      | _ ->
          ( OBJ_ERROR ("Index error: cannot index type " ^ getTypeName left_obj),
            env' ))
  | ListLiteral elems ->
      let evaluated_elems, env' = evalExpressions elems env in
      if
        List.exists
          (function OBJ_ERROR _ -> true | _ -> false)
          evaluated_elems
      then
        ( List.find (function OBJ_ERROR _ -> true | _ -> false) evaluated_elems,
          env' )
      else (OBJ_LIST evaluated_elems, env')
  | IfExpression { condition; consequence; alternative } -> (
      let condition_obj, env' = evalExpression condition env in
      match condition_obj with
      | OBJ_ERROR _ -> (condition_obj, env')
      | _ -> (
          if isTruthy condition_obj then evalStatements consequence env'
          else
            match alternative with
            | Some alt -> evalStatements alt env'
            | None -> (OBJ_NULL, env')))
  | FnExpression { parameters; body } ->
      (OBJ_FUNCTION { parameters; body; env }, env)
  | CallExpression { fn; arguments } -> (
      let fn_obj, env' = evalExpression fn env in
      match fn_obj with
      | OBJ_ERROR _ -> (fn_obj, env')
      | OBJ_BUILTIN name -> (
          let arg_values, env'' = evalExpressions arguments env' in
          match arg_values with
          | [ (OBJ_ERROR _ as err) ] -> (err, env'')
          | _ -> (callBuiltin name arg_values, env''))
      | OBJ_FUNCTION { parameters; body; env = fn_env } -> (
          let arg_values, env'' = evalExpressions arguments env' in
          match arg_values with
          | [ (OBJ_ERROR _ as err) ] -> (err, env'')
          | _ ->
              let param_count = List.length parameters in
              let arg_count = List.length arg_values in
              if param_count <> arg_count then
                ( OBJ_ERROR
                    (Printf.sprintf
                       "Function call error: expected %d argument%s, got %d. \
                        Parameters: [%s]"
                       param_count
                       (if param_count = 1 then "" else "s")
                       arg_count
                       (String.concat ", " parameters)),
                  env'' )
              else
                (* Create new environment with function's closure environment *)
                let extended_env =
                  bindParameters fn_env parameters arg_values
                in
                (* For recursive calls: if calling a named function, bind it in execution environment *)
                let self_referencing_env =
                  match fn with
                  | Ident func_name -> (
                      (* Check if this function exists in the current environment (caller's env) *)
                      match getVar env'' func_name with
                      | Some func_obj when func_obj = fn_obj ->
                          (* It's the same function, add self-reference *)
                          setVar extended_env func_name fn_obj
                      | _ -> extended_env)
                  | _ -> extended_env
                in
                let result, _ = evalStatements body self_referencing_env in
                (* Handle return values - unwrap OBJ_RETURN *)
                let final_result =
                  match result with OBJ_RETURN value -> value | other -> other
                in
                (final_result, env''))
      | _ -> (OBJ_ERROR "not a function: cannot call non-function value", env))

and evalExpressions exprs env =
  let rec aux exprs env acc =
    match exprs with
    | [] -> (List.rev acc, env)
    | expr :: rest -> (
        let expr_val, env' = evalExpression expr env in
        match expr_val with
        | OBJ_ERROR _ -> ([ expr_val ], env')
        | _ -> aux rest env' (expr_val :: acc))
  in
  aux exprs env []

and evalStatement stmt env =
  match stmt with
  | ExpressionStatement expr -> evalExpression expr env
  | BlockStatement block -> evalStatements block env
  | ReturnStatement { value } -> (
      let value_obj, env' = evalExpression value env in
      match value_obj with
      | OBJ_ERROR _ -> (value_obj, env')
      | _ -> (OBJ_RETURN value_obj, env'))
  | LetStatement { name; value } -> (
      let value_obj, env' = evalExpression value env in
      match value_obj with
      | OBJ_ERROR _ -> (value_obj, env')
      | _ ->
          let env'' = setVar env' name value_obj in
          (OBJ_NULL, env''))

and evalStatements statements env =
  let rec eval_stmts stmts env =
    match stmts with
    | [] -> (OBJ_NULL, env)
    | stmt :: rest ->
        let result, env' = evalStatement stmt env in
        if rest = [] then (result, env') else eval_stmts rest env'
  in
  eval_stmts statements env
