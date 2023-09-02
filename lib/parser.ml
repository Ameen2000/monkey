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

let prec_gte a b = Precedence.compare a b >= 0
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

let peek_is parser token =
  Option.equal Token.equal parser.peek_token (Some token)
;;

let expect_assign parser =
  expect_peek parser (function
    | Token.Assign -> true
    | _ -> false)
;;

let expect_lparen parser =
  expect_peek parser (function
    | Token.LParen -> true
    | _ -> false)
;;

let expect_rparen parser =
  expect_peek parser (function
    | Token.RParen -> true
    | _ -> false)
;;

let expect_lbrace parser =
  expect_peek parser (function
    | Token.LBrace -> true
    | _ -> false)
;;

let expect_rbrace parser =
  expect_peek parser (function
    | Token.RBrace -> true
    | _ -> false)
;;

let expect_semicolon parser =
  expect_peek parser (function
    | Token.Semicolon -> true
    | _ -> false)
;;

let expect_comma parser =
  expect_peek parser (function
    | Token.Comma -> true
    | _ -> false)
;;

let peek_precedence parser =
  match parser.peek_token with
  | Some tk -> Precedence.token_prec tk
  | _ -> Precedence.Lowest
;;

let curr_precedence parser =
  match parser.current_token with
  | Some tk -> Precedence.token_prec tk
  | _ -> Precedence.Lowest
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

let rec parse_expression parser prec =
  let* parser, left = parse_prefix_expr parser in
  let rec aux parser left =
    let peeked = parser.peek_token |> Option.value ~default:Token.Illegal in
    let prec_peek = Precedence.token_prec peeked in
    if peek_is parser Token.Semicolon || prec_gte prec prec_peek
    then Ok (parser, left)
    else (
      match get_infix_fn parser with
      | Some infix_fn ->
        let parser = advance parser in
        let* parser, left = infix_fn parser left in
        aux parser left
      | None -> Ok (parser, left))
  in
  aux parser left

and expr_parse_literal parser =
  match parser.current_token with
  | Some (Token.Ident identifier) -> Ok (parser, Ast.Identifier { identifier })
  | Some (Token.String str) -> Ok (parser, Ast.String str)
  | Some (Token.Int number) ->
    let number =
      try Int.of_string number with
      | Failure x -> Fmt.failwith "COULD NOT PARSE %s DUE to %s" number x
    in
    Ok (parser, Ast.Int number)
  | Some Token.True -> Ok (parser, Ast.Boolean true)
  | Some Token.False -> Ok (parser, Ast.Boolean false)
  | _ -> Error "expecting identifier or literal"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser Precedence.Prefix in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_grouped parser =
  let parser = advance parser in
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let* parser = expect_rparen parser in
  Ok (parser, expression)

and expr_parse_fn parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek_token with
    | Some Token.RParen -> parse_list_parameters parser []
    | Some (Token.Ident _) ->
      let parser = advance parser in
      let* identifier = read_identifier parser in
      parse_list_parameters parser [ identifier ]
    | _ -> Error "missing ) to close function parameters"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })

and read_identifier parser =
  match parser.current_token with
  | Some (Token.Ident identifier) -> Ok Ast.{ identifier }
  | _ -> Error "expected to read identifier"

and parse_list_parameters parser accum =
  match parser.peek_token with
  | Some Token.RParen -> Ok (advance parser, List.rev accum)
  | Some Token.Comma ->
    let parser = advance parser |> advance in
    let* ident = read_identifier parser in
    parse_list_parameters parser (ident :: accum)
  | Some _ -> Error "unexpected token"
  | None -> Error "unexpected end of stream"

and parse_prefix_expr parser =
  match parser.current_token with
  | None -> Error "No current token"
  | Some tk ->
    (match tk with
     | Token.Ident _ -> expr_parse_literal parser
     | Token.String _ -> expr_parse_literal parser
     | Token.Int _ -> expr_parse_literal parser
     | Token.Bang -> expr_parse_prefix parser tk
     | Token.Minus -> expr_parse_prefix parser tk
     | Token.True -> expr_parse_literal parser
     | Token.False -> expr_parse_literal parser
     | Token.LParen -> expr_parse_grouped parser
     | Token.If -> parse_if parser
     | Token.Function -> expr_parse_fn parser
     | _ -> Error "more to come")

and get_infix_fn parser =
  match parser.peek_token with
  | Some Token.Plus
  | Some Token.Minus
  | Some Token.Slash
  | Some Token.Asterisk
  | Some Token.Equal
  | Some Token.Not_Equal
  | Some Token.LT
  | Some Token.GT -> Some parse_infix_expression
  | Some Token.LParen -> Some parse_call_expression
  | _ -> None

and parse_infix_expression parser left =
  let operator = parser.current_token |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and parse_call_expression parser fn =
  parse_list_expression parser ~close:Token.RParen ~final:(fun args ->
    Ast.Call { fn; args })

and parse_list_expression parser ~close ~final =
  let rec aux parser exprs =
    match parser.peek_token with
    | Some tk when phys_equal close tk ->
      Ok (advance parser, final (List.rev exprs))
    | Some Token.Comma ->
      let parser = advance parser |> advance in
      let* parser, expr = parse_expression parser Precedence.Lowest in
      aux parser (expr :: exprs)
    | _ -> Error "unexpected next token"
  in
  match parser.peek_token with
  | Some tk when phys_equal close tk -> aux parser []
  | Some _ ->
    let parser = advance parser in
    let* parser, expr = parse_expression parser Precedence.Lowest in
    aux parser [ expr ]
  | None -> Error "EOF"

and parse_return parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser Precedence.Lowest in
  let parser = to_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_let parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  let parser = advance parser in
  let* parser, value = parse_expression parser Precedence.Lowest in
  let parser = to_semicolon parser in
  Ok (parser, Ast.Let { name; value })

and parse_expression_statement parser =
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = to_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expression)

and parse_statement parser =
  match parser.current_token with
  | None -> Error "no more tokens"
  | Some Token.Let -> parse_let parser
  | Some Token.Return -> parse_return parser
  | _ -> parse_expression_statement parser

and parse_block parser =
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

and parse_if parser =
  let* parser = expect_lparen parser in
  let parser = advance parser in
  let* parser, condition = parse_expression parser Precedence.Lowest in
  let* parser = expect_rparen parser in
  let* parser = expect_lbrace parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek_token with
    | Some Token.Else ->
      let parser = advance parser in
      let* parser = expect_lbrace parser in
      let* parser, block = parse_block parser in
      Ok (parser, Some block)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })
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

let parse input =
  let lexer = Lexer.init input in
  let parser = init lexer in
  parse_program parser
;;
