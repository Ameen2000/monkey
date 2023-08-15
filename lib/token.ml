type t =
  | Illegal of string
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of string
  (* Operators *)
  | Assign
  | Plus
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

let lookup_ident keyword =
  match keyword with
  | "fn" -> Function
  | "let" -> Let
  | _ -> Ident keyword
;;
