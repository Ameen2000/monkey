open Core

let repl =
  let terminator = "#quit" in
  let run = ref true in
  while !run do
    let prompt = Out_channel.print_string ">>" in
    prompt;
    let user_input = In_channel.(input_line_exn stdin) in
    if String.(equal user_input terminator)
    then run := false
    else (
      let tokens = Lexer.collect_tokens user_input |> List.map ~f:Token.show in
      List.iter ~f:print_endline tokens)
  done
