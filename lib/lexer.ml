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
    else
      lexer.ch <- Some (String.get lexer.input lexer.readPositon)
  in
  next_char;
  lexer.position <- lexer.readPositon;
  lexer.readPositon <- lexer.readPositon + 1
;;

let init input =
  if String.is_empty input
  then {input; position = 0; readPositon = 0; ch = None}
  else {input; position = 0; readPositon = 0; ch = Some (String.get input 0)}
;;

let token_of_char lexer =
  let aux lexer =
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
        | ch -> Fmt.failwith "unknown character: %c" ch
        in
        Some token
  in
  read_char lexer;
  aux lexer
;;
