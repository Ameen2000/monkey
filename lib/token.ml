type t =
  | Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of string
  | String of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | Equal
  | Not_Equal
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq]

let lookup_ident keyword =
  match keyword with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident keyword
;;
