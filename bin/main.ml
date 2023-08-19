open Monkey
open Core

let () = 
  let input = "+= 12 let 5 4 ();,{}fn x\nlet x = 5 + 5; -/!<>*" in
  let tokens = Lexer.collect_tokens input |> List.map ~f:(Token.show) in
  List.iter ~f:print_endline tokens
