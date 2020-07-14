@ocaml.doc(" hello ") @genType
let flat = 34

@ocaml.doc(
  "
  * Sign a message with a key.
  *
  * @param message - A message to be signed
  * @param key - The key with which to sign the message
  * @returns A signed message
 "
)
@genType
let signMessage = (. message, key) => message ++ string_of_int(key)

export one = a => a + 0

export two = (a, b) => a + b + 0

export tree = (a, b, c) => a + b + c + 0

export oneU = (. a) => a + 0

export twoU = (. a, b) => a + b + 0

export treeU = (. a, b, c) => a + b + c + 0

export useParam = param => param + 34

export useParamU = (. param) => param + 34

export unnamed1 = (_: int) => 34

export unnamed1U = (. _: int) => 34

export unnamed2 = (_: int, _: int) => 34

export unnamed2U = (. _: int, _: int) => 34

export grouped = (~x, ~y, a, b, c, ~z) => x + y + a + b + c + z

export unitArgWithoutConversion = () => "abc"

export unitArgWithoutConversionU = (. ()) => "abc"

type t =
  | A
  | B

export unitArgWithConversion = () => A

export unitArgWithConversionU = (. ()) => A
