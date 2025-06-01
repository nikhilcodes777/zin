open Tokens
open Ast

let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  | LOWEST
  | EQUALITY (* == != >= > < <= *)
  | SUM (* + - *)
  | PRODUCT (* * / *)
  | PREFIX (* -X !X *)
  | CALL (* foo(x,y) *)
[@@deriving show]

type parser = { tokens : token list; current : token; peek : token }
[@@deriving show]

type parse_error =
  | UnexpectedToken of { expected : string; got : token }
  | NoPrefixFunction of token
  | UnexpectedEOF
  | NoClosingBrace
[@@deriving show]

type 'a parse_result = ('a * parser, parse_error) result

let tokenPrecedence = function
  | PLUS | MINUS -> SUM
  | ASTERISK | SLASH -> PRODUCT
  | EQUAL | GT | GT_EQUAL | LT | LT_EQUAL | NOT_EQUAL -> EQUALITY
  | LPAREN -> CALL
  | _ -> LOWEST

let peekPrecedence p = tokenPrecedence p.peek

let newParser = function
  | current :: peek :: rest -> { tokens = peek :: rest; current; peek }
  | [ current ] -> { tokens = []; current; peek = EOF }
  | [] -> { tokens = []; current = EOF; peek = EOF }

let advance p =
  match p.tokens with
  | next :: rest -> (
      match rest with
      | peek :: _ -> { tokens = rest; current = next; peek }
      | [] -> { tokens = []; current = next; peek = EOF })
  | [] -> { tokens = []; current = EOF; peek = EOF }

let expectCurrent p tok =
  if p.current == tok then Ok (advance p)
  else Error (UnexpectedToken { expected = describeToken tok; got = p.current })

let expectSemicolon p =
  match p.current with
  | SEMICOLON -> Ok (advance p)
  | EOF -> Ok (advance p)
  | _ ->
      Error
        (UnexpectedToken { expected = describeToken SEMICOLON; got = p.current })

let parseIntegerLiteral p =
  match p.current with
  | INT value -> Ok (IntLiteral value, advance p)
  | _ -> Error (UnexpectedToken { expected = "INT"; got = p.current })

let parseStringLiteral p =
  match p.current with
  | STRING str -> Ok (StringLiteral str, advance p)
  | _ -> Error (UnexpectedToken { expected = "STRING"; got = p.current })

let parseIdentifier p =
  match p.current with
  | IDENTIFIER name -> Ok (Ident name, advance p)
  | _ -> Error (UnexpectedToken { expected = "IDENTIFIER"; got = p.current })

let parseBooleanLiteral p =
  match p.current with
  | TRUE -> Ok (BoolLiteral true, advance p)
  | FALSE -> Ok (BoolLiteral false, advance p)
  | _ -> Error (UnexpectedToken { expected = "Boolean"; got = p.current })

let rec parseExpression p precedence =
  let* left, p =
    match p.current with
    | INT _ -> parseIntegerLiteral p
    | STRING _ -> parseStringLiteral p
    | IDENTIFIER _ -> parseIdentifier p
    | TRUE | FALSE -> parseBooleanLiteral p
    | BANG | MINUS -> parsePrefixExpression p
    | LPAREN -> parseGroupedExpression p
    | IF -> parseIfExpression p
    | FUNCTION -> parseFnExpression p
    | _ -> Error (NoPrefixFunction p.current)
  in
  let rec parseInfixLoop left p =
    if
      p.current = SEMICOLON || p.current = EOF || p.current = COMMA
      || tokenPrecedence p.current <= precedence
    then Ok (left, p)
    else
      match p.current with
      | PLUS | MINUS | ASTERISK | SLASH | EQUAL | LT | LT_EQUAL | GT | GT_EQUAL
      | NOT_EQUAL ->
          let* left', p = parseInfixExpression left p in
          parseInfixLoop left' p
      | LPAREN ->
          let* left', p = parseCallExpression left p in
          parseInfixLoop left' p
      | _ -> Ok (left, p)
  in
  parseInfixLoop left p

and parseGroupedExpression p =
  let p = advance p in
  let* expr, p = parseExpression p LOWEST in
  let* p = expectCurrent p RPAREN in
  Ok (expr, p)

and parsePrefixExpression p =
  let operator = p.current in
  let p = advance p in
  let* right, p = parseExpression p PREFIX in
  Ok (Prefix { operator; right }, p)

and parseInfixExpression left p =
  let operator = p.current in
  let precedence = tokenPrecedence p.current in
  let p = advance p in
  let* right, p = parseExpression p precedence in
  Ok (Infix { left; operator; right }, p)

and parseIfExpression p =
  let p = advance p in
  let* p = expectCurrent p LPAREN in
  let* condition, p = parseExpression p LOWEST in
  let* p = expectCurrent p RPAREN in
  let* consequence, p = parseBlockStatement p in
  if p.current <> ELSE then
    Ok (IfExpression { condition; consequence; alternative = None }, advance p)
  else
    let p = advance p in
    let* alternative, p = parseBlockStatement p in
    Ok
      ( IfExpression { condition; consequence; alternative = Some alternative },
        p )

and parseFnExpression p =
  let rec parseFnParams p params =
    match p.current with
    | RPAREN -> Ok (List.rev params, p)
    | IDENTIFIER param ->
        let p = advance p in
        if p.current = COMMA then parseFnParams (advance p) (param :: params)
        else parseFnParams p (param :: params)
    | _ -> Error (UnexpectedToken { expected = "parameter"; got = p.current })
  in

  let p = advance p in
  let* p = expectCurrent p LPAREN in
  let* parameters, p = parseFnParams p [] in
  if p.current <> RPAREN then
    Error (UnexpectedToken { expected = ")"; got = p.current })
  else
    let p = advance p in
    let* body, p = parseBlockStatement p in
    Ok (FnExpression { parameters; body }, p)

and parseCallExpression fn p =
  let rec parseCallArgs p args =
    match p.current with
    | RPAREN -> Ok (List.rev args, p)
    | _ ->
        let* expr, p = parseExpression p LOWEST in
        if p.current = COMMA then parseCallArgs (advance p) (expr :: args)
        else parseCallArgs p (expr :: args)
  in

  let p = advance p in
  let* arguments, p = parseCallArgs p [] in
  let* p = expectCurrent p RPAREN in
  Ok (CallExpression { fn; arguments }, p)

and parseBlockStatement p =
  let* p = expectCurrent p LBRACE in
  let rec parseStmts p stmts =
    match p.current with
    | RBRACE -> Ok (List.rev stmts, advance p)
    | EOF -> Error NoClosingBrace
    | _ ->
        let* stmt, p = parseStatement p in
        parseStmts p (stmt :: stmts)
  in
  parseStmts p []

and parseStatement p =
  match p.current with
  | LET -> parseLetStatement p
  | RETURN -> parseReturnStatement p
  | _ ->
      (*Semicolon is optional ExpressionStatement*)
      let* stmt, p = parseExpressionStatement p in
      let p = if p.current = SEMICOLON then advance p else p in
      Ok (stmt, p)

and parseLetStatement p =
  let p = advance p in
  match p.current with
  | IDENTIFIER name ->
      let p = advance p in
      let* p = expectCurrent p ASSIGN in
      let* value, p = parseExpression p LOWEST in
      let* p = expectSemicolon p in
      Ok (LetStatement { name; value }, p)
  | _ -> Error (UnexpectedToken { expected = "identifier"; got = p.current })

and parseReturnStatement p =
  let p = advance p in
  let* value, p = parseExpression p LOWEST in
  let* p = expectSemicolon p in
  Ok (ReturnStatement { value }, p)

and parseExpressionStatement p =
  let* expr, p = parseExpression p LOWEST in
  Ok (ExpressionStatement expr, p)

let parseProgram p =
  let rec parseStmts p stmts =
    match p.current with
    | EOF -> Ok (List.rev stmts)
    | _ ->
        let* stmt, p = parseStatement p in
        parseStmts p (stmt :: stmts)
  in
  parseStmts p []

let test_parse_program input =
  Printf.printf "Parsing Program:\n\"%s\"\n" input;
  let tokens = Lexer.lex input in
  let parser = newParser tokens in
  match parseProgram parser with
  | Ok stmts ->
      Printf.printf "\nParse successful!\n";
      Printf.printf "Parsed %d statement(s):\n" (List.length stmts);
      List.iteri
        (fun i stmt ->
          Printf.printf "\nStatement %d: %s\n" (i + 1) (show_statement stmt))
        stmts
  | Error err -> Printf.printf "\nParse failed: %s\n" (show_parse_error err)
