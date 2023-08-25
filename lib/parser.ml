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

let parse_identifier parser =
  let open Token in
  match parser.peek_token with
  | Some (Ident identifier) -> Ok (advance parser, Ast.{identifier})
  | _ -> Error "Expected identifier after let"
;;

let parse_expression parser =
  match parser.peek_token with
  | Some Token.Int number -> Ok (advance parser, Int.of_string number)
  | _ -> Error "Expected Int"
;;

let parse_return parser =
  match parser.peek_token with
  | Some Token.Return ->
      let* _, value = parse_expression (advance parser) in
      Ok (advance parser, value)
  | _ -> Error "Expected return"

(**
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
  Ok (Ast.Program {statements})

and parse_statement parser =
  match parser.current_token with
  | None -> Error "no more tokens"
  | Some Token.Let -> parse_let parser
    | _ -> assert false
 **)
