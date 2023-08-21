type t =
  { lexer : Lexer.t
  ; current_token : Token.t option
  ; peek_token : Token.t option
  }
[@@deriving show]

let init lexer =
  let current_lexer = lexer in
  let _, current_token = Lexer.next_token lexer in
  let _, peek_token = Lexer.next_token lexer in
  { lexer = current_lexer; current_token; peek_token }
;;

let advance parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; current_token = parser.peek_token; peek_token = peek }
;;

let next_token parser =
  let parser = advance parser in
  parser, parser.current_token
;;

(** let parse_program parser =
    let rec aux statements =
    match parser.current_token with
    | Token.EOF -> statements
    | Token.Let -> aux ((parse_let token) :: statements)
    | Token.Return -> aux ((parse_return token) :: statements)
    | Token.If -> aux ((parse_condition token) :: statements)
    and parse_statement t = ...
    and parse_return t = ...
    and parse_condition t = ...
    **)
