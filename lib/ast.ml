open Tokens

type identifier = string [@@deriving show]

and expression =
  | IntLiteral of int
  | BoolLiteral of bool
  | Ident of identifier
  | Prefix of { operator : token; right : expression }
  | Infix of { left : expression; operator : token; right : expression }
  | IfExpression of {
      condition : expression;
      consequence : block;
      alternative : block option;
    }
  | FnExpression of { parameters : identifier list; body : block }
  | CallExpression of { fn : expression; arguments : expression list }
[@@deriving show]

and block = statement list [@@deriving show]

and statement =
  | LetStatement of { name : identifier; value : expression }
  | ExpressionStatement of expression
  | ReturnStatement of { value : expression }
  | BlockStatement of block
[@@deriving show]

and program = statement list [@@deriving show]

and node =
  | Program of program
  | Statement of statement
  | Expression of expression
[@@deriving show]
