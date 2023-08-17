open Core

type t =
  { input : string
  ; mutable position : int
  ; mutable read_position : int
  ; mutable ch : char option
  }

let advance lexer =
  let next_char =
    if lexer.read_position >= String.length lexer.input - 1
    then lexer.ch <- None
    else lexer.ch <- Some (String.get lexer.input lexer.read_position)
  in
  next_char;
  lexer.position <- lexer.read_position;
  lexer.read_position <- lexer.read_position + 1
;;

let init input =
  if String.is_empty input
  then { input; position = 0; read_position = 0; ch = None }
  else
    { input; position = 0; read_position = 0; ch = Some (String.get input 0) }
;;

let read_while lexer =
  let positon = lexer.position in
  let rec aux ch l =
    match ch with
    | None -> ()
    | Some x ->
      (match Char.is_alpha x with
       | true -> advance l
       | _ -> ())
  in
  aux lexer.ch lexer;
  String.sub ~pos:positon ~len:(lexer.position - positon) lexer.input
;;

let skip_whitespace lexer =
  let rec aux lexer =
    let condition =
      match lexer.ch with
      | None -> false
      | Some ch -> Char.(ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r')
    in
    if condition
    then (
      advance lexer;
      aux lexer)
    else lexer
  in
  aux lexer
;;

let read_identifier lexer = read_while lexer |> Token.lookup_ident
let read_number lexer = Token.Int (read_while lexer)

let next_token lexer =
  let token_of_char lexer =
    let is_identifer ch = Char.(ch = '_' || is_alpha ch) in
    let is_number ch = Char.is_digit ch in
    let lexer = skip_whitespace lexer in
    match lexer.ch with
    | None -> None
    | Some ch ->
      let token =
        match ch with
        | '=' -> Token.Assign
        | ';' -> Token.Semicolon
        | '(' -> Token.LParen
        | ')' -> Token.RParen
        | ',' -> Token.Comma
        | '+' -> Token.Plus
        | '{' -> Token.LBrace
        | '}' -> Token.RBrace
        | '\000' -> Token.EOF
        | ch ->
          if is_identifer ch
          then read_identifier lexer
          else if is_number ch
          then read_number lexer
          else Fmt.failwith "unkown char %c" ch
      in
      Some token
  in
  advance lexer;
  token_of_char lexer
;;
