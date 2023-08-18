open Core

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }
[@@deriving show]

let advance lexer =
  if lexer.read_position >= String.length lexer.input
  then { lexer with ch = None }
  else (
    let position = lexer.read_position in
    let read_position = lexer.read_position + 1 in
    { lexer with position; read_position; ch = Some (String.get lexer.input position) })
;;

let init input =
  if String.is_empty input
  then { input; position = 0; read_position = 0; ch = None }
  else
    { input; position = 0; read_position = 0; ch = Some (String.get input 0) }
;;

let read_while lexer condition =
  let positon = lexer.position in
  let aux lexer condition =
    let rec loop lexer =
      if condition lexer.ch then loop (advance lexer) else lexer
    in
    let lexer = loop lexer in
    lexer, lexer.position
  in
  let lexer, positon_end =
    aux lexer (fun ch ->
      match ch with
      | Some character -> condition character
      | None -> false)
  in
  lexer, String.sub lexer.input ~pos:positon ~len:(positon_end - positon)
;;

let is_number ch = Char.is_digit ch
let is_identifer ch = Char.(ch = '_' || is_alpha ch)

let read_identifier lexer =
  let lexer, ident = read_while lexer is_identifer in
  lexer, Token.lookup_ident ident
;;

let read_number lexer =
  let lexer, number = read_while lexer is_number in
  lexer, Token.Int number
;;

let rec skip_whitespace lexer =
  match lexer.ch with
  | None -> lexer
  | Some character ->
      if Char.is_whitespace character then advance lexer |> skip_whitespace
      else lexer

let next_token lexer =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    let lexer, token =
      match ch with
      | '=' -> advance lexer, Token.Assign
      | ';' -> advance lexer, Token.Semicolon
      | '(' -> advance lexer, Token.LParen
      | ')' -> advance lexer, Token.RParen
      | ',' -> advance lexer, Token.Comma
      | '+' -> advance lexer, Token.Plus
      | '{' -> advance lexer, Token.LBrace
      | '}' -> advance lexer, Token.RBrace
      | '\000' -> advance lexer, Token.EOF
      | ch ->
        if is_identifer ch
        then read_identifier lexer
        else if is_number ch
        then read_number lexer
        else Fmt.failwith "Unkown character: %c" ch
    in
    lexer, Some token
;;
