type t =
  { lexer : Lexer.t
  ; current_token : Token.t option
  ; peek_token : Token.t option
  }
[@@deriving show]

let init lexer =
  let current_lexer = lexer in
  let advanced_lexer, current_token = Lexer.next_token lexer in
  let _, peek_token = Lexer.next_token advanced_lexer in
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

(**
let parse_program parser =
  let rec aux parser statements =
    match parser.current_token with
    | None -> Ast.Program {statements = List.rev statements}
    | Some tk ->
        (match tk with
        | Token.EOF -> Ast.Program {statements = List.rev statements}
        | _ -> 
            let stmt = parse_statement parser in
            aux (advance parser) (stmt :: statements))
            in
  aux parser []

and parse_statement parser =
  match parser.current_token with
  | None -> assert false
  | Some Token.Let -> parse_let parser
    | _ -> assert false

and parse_let parser = ...
 **)
