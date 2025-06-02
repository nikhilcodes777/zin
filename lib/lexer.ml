type lexer = {
  input : string;
  mutable readPosition : int;
  mutable position : int;
  mutable ch : char;
}

(* Utility functions *)
let isLetter (ch : char) : bool =
  Base.Char.is_alpha ch || Base.Char.equal ch '_'

let readChar l =
  if l.readPosition >= Base.String.length l.input then l.ch <- '\000'
  else l.ch <- Base.String.get l.input l.readPosition;
  l.position <- l.readPosition;
  l.readPosition <- l.readPosition + 1

let lexerNew input =
  let l = { input; position = 0; readPosition = 0; ch = '\000' } in
  let () = l.readPosition <- 0 in
  readChar l;
  l

let peekLexer l =
  if l.readPosition >= Base.String.length l.input then '\000'
  else String.get l.input l.readPosition

let skipWhiteSpace l =
  while Base.Char.is_whitespace l.ch && not (Base.Char.equal l.ch '\000') do
    readChar l
  done

let skipComment l =
  while
    (not (Base.Char.equal l.ch '\n')) && not (Base.Char.equal l.ch '\000')
  do
    readChar l
  done

let readString l =
  let position = l.position + 1 in
  readChar l;
  while (not (Base.Char.equal l.ch '"')) && not (Base.Char.equal l.ch '\000') do
    readChar l
  done;
  if Base.Char.equal l.ch '"' then (
    let str =
      Base.String.sub l.input ~pos:position ~len:(l.position - position)
    in
    readChar l;
    Tokens.STRING str)
  else Tokens.ILLEGAL

let readIdentifier l =
  let position = l.position in
  while isLetter l.ch do
    readChar l
  done;
  Tokens.keywordsLookup
    (Base.String.sub l.input ~pos:position ~len:(l.position - position))

let readInteger l =
  let position = l.position in
  while Base.Char.is_digit l.ch do
    readChar l
  done;
  Tokens.INT
    (Base.Int.of_string
    @@ Base.String.sub l.input ~pos:position ~len:(l.position - position))

let rec nextToken (l : lexer) : Tokens.token =
  skipWhiteSpace l;
  match l.ch with
  | '=' ->
      if Base.Char.equal (peekLexer l) '=' then (
        readChar l;
        readChar l;
        Tokens.EQUAL)
      else (
        readChar l;
        Tokens.ASSIGN)
  | '+' ->
      readChar l;
      Tokens.PLUS
  | '-' ->
      if Base.Char.equal (peekLexer l) '-' then (
        readChar l;
        readChar l;
        skipComment l;
        nextToken l)
      else (
        readChar l;
        Tokens.MINUS)
  | '"' -> readString l
  | ';' ->
      readChar l;
      Tokens.SEMICOLON
  | ',' ->
      readChar l;
      Tokens.COMMA
  | '.' ->
      if Base.Char.equal (peekLexer l) '.' then (
        readChar l;
        readChar l;
        Tokens.RANGE)
      else (
        readChar l;
        Tokens.DOT)
  | '(' ->
      readChar l;
      Tokens.LPAREN
  | ')' ->
      readChar l;
      Tokens.RPAREN
  | '[' ->
      readChar l;
      Tokens.LBRACKET
  | ']' ->
      readChar l;
      Tokens.RBRACKET
  | '{' ->
      readChar l;
      Tokens.LBRACE
  | '}' ->
      readChar l;
      Tokens.RBRACE
  | '*' ->
      readChar l;
      Tokens.ASTERISK
  | '<' ->
      if Base.Char.equal (peekLexer l) '=' then (
        readChar l;
        readChar l;
        Tokens.LT_EQUAL)
      else (
        readChar l;
        Tokens.LT)
  | '>' ->
      if Base.Char.equal (peekLexer l) '=' then (
        readChar l;
        readChar l;
        Tokens.GT_EQUAL)
      else (
        readChar l;
        Tokens.GT)
  | '/' ->
      readChar l;
      Tokens.SLASH
  | '!' ->
      if Base.Char.equal (peekLexer l) '=' then (
        readChar l;
        readChar l;
        Tokens.NOT_EQUAL)
      else (
        readChar l;
        Tokens.BANG)
  | '\000' -> Tokens.EOF
  | ch when isLetter ch -> readIdentifier l
  | ch when Base.Char.is_digit ch -> readInteger l
  | _ -> Tokens.ILLEGAL

let lex input =
  let rec collect_tokens lexer acc =
    let tok = nextToken lexer in
    if tok = Tokens.EOF then List.rev (tok :: acc)
      (* include EOF as the last token *)
    else collect_tokens lexer (tok :: acc)
  in
  collect_tokens (lexerNew input) []
