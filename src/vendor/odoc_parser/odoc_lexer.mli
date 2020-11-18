type input = {
  file : string;
  offset_to_location : int -> Location_.point;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Location_.with_location
