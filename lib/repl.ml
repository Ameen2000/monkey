open Core

let ( let* ) res f = Result.bind res ~f

let repl =
  let terminator = "#quit" in
  let run = ref true in
  while !run do
    Format.printf ">> ";
    Format.print_flush ();
    let user_input = In_channel.(input_line_exn stdin) in
    if String.(equal user_input terminator)
    then run := false
    else (
      let ast = Parser.parse user_input |> Result.ok |> Option.value_exn in
      let string_of_ast = Ast.show ast in
      print_endline string_of_ast
      )
  done
;;
