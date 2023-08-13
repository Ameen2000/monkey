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
