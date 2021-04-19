@genType.import("./bsString")
external pack: (@string [@as("remove-range") #removeRange | #normal], string, string) => string =
  "pack"

let x = pack(#removeRange, "a", "b")

