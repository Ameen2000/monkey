type t =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char option
  }

val init : string -> t
val next_token : t -> t * Token.t option
