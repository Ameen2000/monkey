open Core

type t =
  { input : string
  ; mutable position : int
  ; mutable readPositon : int
  ; mutable ch : char option
  }

let read_char lexer =
  let next_char =
    if lexer.readPositon >= String.length lexer.input
    then lexer.ch <- Some '\000'
    else lexer.ch <- Some (String.get lexer.input lexer.readPositon)
  in
  next_char;
  lexer.position <- lexer.readPositon;
  lexer.readPositon <- lexer.readPositon + 1
;;

let init input =
  if String.is_empty input
  then { input; position = 0; readPositon = 0; ch = None }
  else { input; position = 0; readPositon = 0; ch = Some (String.get input 0) }
;;

let read_while lexer =
  let positon = lexer.position in
  let rec aux ch l =
    match ch with
    | None -> ()
    | Some x ->
      (match Char.is_alpha x with
       | true -> read_char l
       | _ -> ())
  in
  aux lexer.ch lexer;
  String.sub ~pos:positon ~len:(lexer.position - positon) lexer.input
;;

let read_identifier lexer = read_while lexer |> Token.lookup_ident
let read_number lexer = Token.Int (read_while lexer)

let next_token lexer =
  let token_of_char lexer =
    let is_identifer ch = Char.(ch = '-' || is_alpha ch) in
    let is_number ch = Char.is_digit ch in
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
  read_char lexer;
  token_of_char lexer
;;
