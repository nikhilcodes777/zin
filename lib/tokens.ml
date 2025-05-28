type token =
  | IDENTIFIER of string
  | INT of int
  | ASSIGN
  | PLUS
  | MINUS
  | COMMA
  | ASTERISK
  | BANG
  | SLASH
  | LT
  | GT
  | LT_EQUAL
  | GT_EQUAL
  | EQUAL
  | NOT_EQUAL
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LET
  | FUNCTION
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
  | ILLEGAL
  | EOF

let keywordsLookup word =
  match word with
  | "let" -> LET
  | "fn" -> FUNCTION
  | "true" -> TRUE
  | "false" -> FALSE
  | "if" -> IF
  | "else" -> ELSE
  | "return" -> RETURN
  | _ -> IDENTIFIER word

let describeToken = function
  | ILLEGAL -> "Illegal"
  | EOF -> "End of File"
  | IDENTIFIER s -> "Identifier: " ^ s
  | INT n -> "Integer: " ^ string_of_int n
  | ASSIGN -> "Assign"
  | PLUS -> "Plus"
  | MINUS -> "Minus"
  | ASTERISK -> "Asterisk"
  | LT -> "LT"
  | GT -> "GT"
  | LT_EQUAL -> "<="
  | GT_EQUAL -> ">="
  | EQUAL -> "=="
  | NOT_EQUAL -> "!="
  | SLASH -> "SLash"
  | BANG -> "Bang"
  | COMMA -> "Comma"
  | SEMICOLON -> "Semicolon"
  | LPAREN -> "Left Paren"
  | RPAREN -> "Right Paren"
  | LBRACE -> "Left Brace"
  | RBRACE -> "Right Brace"
  | LET -> "Let"
  | FUNCTION -> "Function"
  | TRUE -> "True"
  | FALSE -> "False"
  | IF -> "If"
  | ELSE -> "Else"
  | RETURN -> "Return"
