open Core

type t =
  { input : string
  ; mutable position : int
  ; mutable readPositon : int
  ; mutable ch : char
  }

let read_char lexer =
  let next_char =
    if lexer.readPositon >= String.length lexer.input
    then lexer.ch <- char_of_int 0
    else
      lexer.ch
        <- (let string_list = lexer.input |> String.to_list in
            List.nth_exn string_list lexer.readPositon)
  in
  next_char;
  lexer.position <- lexer.readPositon;
  lexer.readPositon <- lexer.readPositon + 1
;;

let next l =
  let char_of_input = l |> String.to_list |> List.hd_exn in
  let lexer =
    { input = l; position = 0; readPositon = 0; ch = char_of_input }
  in
  read_char lexer;
  lexer
;;

let token_of_char lexer =
  let aux lexer =
    match lexer.ch with
    | '=' -> Token.Assign
    | ';' -> Token.Semicolon
    | '(' -> Token.LParen
    | ')' -> Token.RParen
    | ',' -> Token.Comma
    | '+' -> Token.Plus
    | '{' -> Token.LBrace
    | '}' -> Token.RBrace
    | '\000' -> Token.EOF
    | _ -> assert false
  in
  read_char lexer;
  aux lexer
;;
