open Monkey
open Core

let () = 
  let input = "+=12let();,{}fn" in
  let tokens = Lexer.collect_tokens input |> List.map ~f:(Token.show) in
  List.iter ~f:print_endline tokens
