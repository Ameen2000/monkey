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
      let ast =
        let tr = Parser.parse user_input in
        match tr with
        | Ok tr -> Ast.show tr
        | Error msg -> msg.msg
      in
      print_endline ast)
  done
;;
