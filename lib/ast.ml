type t =
  | Program of program
  | Statement of statement
  | Expression of expression

and program = { statements : statement list }

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression

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
      ; consequence : program
      ; alternative : program option
      }
  | FunctionLiteral of
      { parameter : identifier list
      ; body : program
      }

and identifier = { identifier : string } [@@deriving show]

let token_literal token =
  match token with
  | Program _ -> "program"
  | Expression _ -> "expressions"
  | Statement _ -> "statements"
;;
