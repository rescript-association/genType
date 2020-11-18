val parse :
  (Token.t Location_.with_location) Stream.t ->
    (Ast.docs, Error.t) Error.result
