type t =
  | Program of program
  | Statement of statement
  | Expression of expression

and program = { statements : statement list }
and block = program

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block

and expression =
  | Identifier of identifier
  | Int of int
  | Boolean of bool
  | String of string
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { operator : Token.t
      ; left : expression
      ; right : expression
      }
  | If of
      { condition : expression
      ; consequence : block
      ; alternative : block option
      }
  | FunctionLiteral of
      { parameters : identifier list
      ; body : block
      }
  | Call of
      { fn : expression
      ; args : expression list
      }

and identifier = { identifier : string } [@@deriving show]

let token_literal token =
  match token with
  | Program _ -> "program"
  | Expression _ -> "expressions"
  | Statement _ -> "statements"
;;
