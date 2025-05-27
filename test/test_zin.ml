open OUnit2
open Zin
open Zin.Tokens

let test_lexer_tokenization _ =
  let input =
    "let x = 5; if (x < 10) { return true; } else { return false; }"
  in
  let expected_tokens =
    [
      Tokens.LET;
      Tokens.IDENTIFIER "x";
      Tokens.ASSIGN;
      Tokens.INT 5;
      Tokens.SEMICOLON;
      Tokens.IF;
      Tokens.LPAREN;
      Tokens.IDENTIFIER "x";
      Tokens.LT;
      Tokens.INT 10;
      Tokens.RPAREN;
      Tokens.LBRACE;
      Tokens.Return;
      Tokens.TRUE;
      Tokens.SEMICOLON;
      Tokens.RBRACE;
      Tokens.ELSE;
      Tokens.LBRACE;
      Tokens.Return;
      Tokens.FALSE;
      Tokens.SEMICOLON;
      Tokens.RBRACE;
      Tokens.EOF;
    ]
  in
  let actual_tokens = Lexer.lex input in
  assert_equal
    ~printer:(fun toks ->
      Base.String.concat ~sep:"\n" (List.map Tokens.describeToken toks))
    expected_tokens actual_tokens

let lexer_basic _ =
  let input = "()+{}=;," in
  let expected : token list =
    [ LPAREN; RPAREN; PLUS; LBRACE; RBRACE; ASSIGN; SEMICOLON; COMMA; EOF ]
  in
  assert_equal expected (Lexer.lex input)

let test_complex_lexer_input _ =
  let input =
    "let five = 5;\n\
     let ten = 10;\n\n\
     let add=fn(x,y) {return x+y;};\n\
     let result = add(five,ten);\n\
     if (five <= ten) {\n\
     return true;\n\
     } else {\n\
     return false;\n\
     }\n\
     10 ==10;\n\
     10 !=10;"
  in

  let expected_tokens =
    [
      Tokens.LET;
      Tokens.IDENTIFIER "five";
      Tokens.ASSIGN;
      Tokens.INT 5;
      Tokens.SEMICOLON;
      Tokens.LET;
      Tokens.IDENTIFIER "ten";
      Tokens.ASSIGN;
      Tokens.INT 10;
      Tokens.SEMICOLON;
      Tokens.LET;
      Tokens.IDENTIFIER "add";
      Tokens.ASSIGN;
      Tokens.FUNCTION;
      Tokens.LPAREN;
      Tokens.IDENTIFIER "x";
      Tokens.COMMA;
      Tokens.IDENTIFIER "y";
      Tokens.RPAREN;
      Tokens.LBRACE;
      Tokens.Return;
      Tokens.IDENTIFIER "x";
      Tokens.PLUS;
      Tokens.IDENTIFIER "y";
      Tokens.SEMICOLON;
      Tokens.RBRACE;
      Tokens.SEMICOLON;
      Tokens.LET;
      Tokens.IDENTIFIER "result";
      Tokens.ASSIGN;
      Tokens.IDENTIFIER "add";
      Tokens.LPAREN;
      Tokens.IDENTIFIER "five";
      Tokens.COMMA;
      Tokens.IDENTIFIER "ten";
      Tokens.RPAREN;
      Tokens.SEMICOLON;
      Tokens.IF;
      Tokens.LPAREN;
      Tokens.IDENTIFIER "five";
      Tokens.LT_EQUAL;
      Tokens.IDENTIFIER "ten";
      Tokens.RPAREN;
      Tokens.LBRACE;
      Tokens.Return;
      Tokens.TRUE;
      Tokens.SEMICOLON;
      Tokens.RBRACE;
      Tokens.ELSE;
      Tokens.LBRACE;
      Tokens.Return;
      Tokens.FALSE;
      Tokens.SEMICOLON;
      Tokens.RBRACE;
      Tokens.INT 10;
      Tokens.EQUAL;
      Tokens.INT 10;
      Tokens.SEMICOLON;
      Tokens.INT 10;
      Tokens.NOT_EQUAL;
      Tokens.INT 10;
      Tokens.SEMICOLON;
      Tokens.EOF;
    ]
  in

  let actual_tokens = Lexer.lex input in

  assert_equal
    ~printer:(fun toks ->
      Base.String.concat ~sep:"\n" (List.map Tokens.describeToken toks))
    expected_tokens actual_tokens

let () =
  run_test_tt_main
    ("Tests"
    >::: [
           "Basic Charactor Lexing " >:: lexer_basic;
           "tokenize_basic_input" >:: test_lexer_tokenization;
           "tokenize_complex_input" >:: test_complex_lexer_input;
         ])
