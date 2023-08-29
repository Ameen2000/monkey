open Core

type t =
  { lexer : Lexer.t
  ; current_token : Token.t option
  ; peek_token : Token.t option
  }
[@@deriving show]

type parse_error =
  { msg : string
  ; parser : t
  ; statements : Ast.statement list
  }
[@@deriving show]

module Precedence = struct
  type t =
    | Lowest
    | Equals
    | LessGreater
    | Sum
    | Product
    | Prefix
    | Call
    | Highest
  [@@deriving show, ord]

  let token_prec token =
    let open Token in
    match token with
    | Equal | Not_Equal -> Equals
    | LT | GT -> LessGreater
    | Plus | Minus -> Sum
    | Slash | Asterisk -> Product
    | LParen -> Call
    | LBrace -> Highest
    | _ -> Lowest
  ;;
end

let ( let* ) res f = Result.bind res ~f
let error_msg parser msg statements = Error { msg; parser; statements }

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

let expect_peek parser condition =
  match parser.peek_token with
  | None -> Error "no peek token"
  | Some tk ->
    if condition tk
    then Ok (advance parser)
    else Error (Fmt.failwith "missing peeked: %a" pp parser)
;;

let expect_assign parser =
  expect_peek parser (function
    | Token.Assign -> true
    | _ -> false)
;;

let rec advance_till parser ~token =
  match parser.current_token with
  | None -> parser
  | Some tk ->
    if Token.(equal tk token)
    then parser
    else advance_till (advance parser) ~token
;;

let to_semicolon parser =
  match parser.peek_token with
  | Some Token.Semicolon -> advance parser
  | _ -> parser
;;

let parse_identifier parser =
  let open Token in
  match parser.peek_token with
  | Some (Ident identifier) -> Ok (advance parser, Ast.{ identifier })
  | _ -> Error "Expected identifier after let"
;;

let expr_parse parser =
  match parser.current_token with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.Identifier { identifier })
  | Some (Token.String str) -> Ok (parser, Ast.String str)
  | Some (Token.Int number) ->
    let number =
      try Int.of_string number with
      | Failure x -> Fmt.failwith "COULD NOT PARSE %s DUE to %s" number x
    in
    Ok (parser, Ast.Int number)
  | Some (Token.True) -> Ok (parser, Ast.Boolean true)
  | Some (Token.False) -> Ok (parser, Ast.Boolean false)
  | _ -> Error "expecting identifier or literal"
;;

let parse_prefix_expr parser =
  match parser.current_token with
  | None -> Error "No current token"
  | Some tk ->
    (match tk with
     | Token.Ident _ -> expr_parse parser
     | Token.String _ -> expr_parse parser
     | Token.Int _ -> expr_parse parser
     | _ -> Error "more to come")
;;

let parse_expression parser prec =
  match prec with
  | Precedence.Lowest -> Ok (parser, Ast.Int 4)
  | _ -> Error "work in progress"


let parse_return parser =
  match parser.peek_token with
  | Some Token.Return ->
    let* parser, value = parse_expression (advance parser) Precedence.Lowest in
    Ok (to_semicolon parser, Ast.Return value)
  | _ -> Error "Expected return"
;;

let parse_let parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  let parser = advance parser in
  let* parser, value = parse_expression parser Precedence.Lowest in
  let parser = to_semicolon parser in
  Ok (parser, Ast.Let { name; value })
;;

let parse_expression_statement parser =
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = to_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expression)
;;

let parse_statement parser =
  match parser.current_token with
  | None -> Error "no more tokens"
  | Some Token.Let -> parse_let parser
  | Some Token.Return -> parse_return parser
  | _ -> parse_expression_statement parser
;;

let parse_block parser =
  let parser = advance parser in
  let rec aux parser statements =
    match parser.current_token with
    | Some Token.RBrace -> Ok (parser, List.rev statements)
    | Some _ ->
      let* parser, statement = parse_statement parser in
      aux (advance parser) (statement :: statements)
    | None -> Error "Missing closing bracket"
  in
  let* parser, program = aux parser [] in
  Ok (parser, Ast.{ statements = program })
;;

let parse_program parser =
  let rec aux parser statements =
    match parser.current_token with
    | None -> Ok (parser, List.rev statements)
    | Some _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> aux (advance parser) (stmt :: statements)
       | Error msg -> error_msg parser msg statements)
  in
  let* _, statements = aux parser [] in
  Ok (Ast.Program { statements })
;;
