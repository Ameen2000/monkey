type t =
  | Program of program
  | Statements of statement
  | Expressions of expression

and program = { statements : statement list }

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression

and expression =
  | Int of int
  | Boolean of bool
  | String of string

and identifier = { identifier : string }
