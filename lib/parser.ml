type t =
  { lexer : Lexer.t
  ; current_token : Token.t option
  ; peek_token : Token.t option
  }

let init lexer =
  let current_lexer = lexer in
  let _, current_token = Lexer.next_token lexer in
  let _, peek_token = Lexer.next_token lexer in
  { lexer = current_lexer; current_token; peek_token }
;;
