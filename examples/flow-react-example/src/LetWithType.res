	
module Style = {
  type rec t =
    | WHITE
    | LIGHT
    | GRAY
    | CUSTOM(string)
}

include Style

@genType.opaque
type rec style = t

@genType
let white: style = WHITE
