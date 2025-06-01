type token =
  | IDENTIFIER of string
  | INT of int
  | STRING of string
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
[@@deriving show]

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
  | STRING s -> "String: " ^ s
  | ASSIGN -> "Assign"
  | PLUS -> "Plus"
  | MINUS -> "Minus"
  | ASTERISK -> "Asterisk"
  | LT -> ">"
  | GT -> "<"
  | LT_EQUAL -> "<="
  | GT_EQUAL -> ">="
  | EQUAL -> "=="
  | NOT_EQUAL -> "!="
  | SLASH -> "/"
  | BANG -> "!"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LET -> "let"
  | FUNCTION -> "fn"
  | TRUE -> "true"
  | FALSE -> "false"
  | IF -> "if"
  | ELSE -> "else"
  | RETURN -> "return"
