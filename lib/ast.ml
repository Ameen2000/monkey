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

and identifier = { identifier : string } [@@deriving show]

let token_literal token =
  match token with
  | Program _ -> "program"
  | Expression _ -> "expressions"
  | Statement _ -> "statements"
;;
