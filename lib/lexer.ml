open Core

type t =
  { input : string
  ; mutable position : int
  ; mutable read_position : int
  ; mutable ch : char option
  }
[@@deriving show]

let advance lexer =
  let side_effect lexer =
    let check_char =
      if lexer.read_position >= String.length lexer.input
      then lexer.ch <- None
      else lexer.ch <- Some (String.get lexer.input lexer.read_position)
    in
    check_char;
    lexer.position <- lexer.read_position;
    lexer.read_position <- lexer.read_position + 1
  in
  side_effect lexer;
  lexer
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
    if Char.is_whitespace character
    then advance lexer |> skip_whitespace
    else lexer
;;

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

let collect_tokens input =
  let lexer = init input in
  let rec aux lexer accum =
    match lexer.ch with
    | None -> List.rev accum
    | _ ->
        let lx, token = next_token lexer in
        match token with
        | None -> aux lx accum
        | Some tk -> aux lx (tk :: accum)
  in
  aux lexer []
