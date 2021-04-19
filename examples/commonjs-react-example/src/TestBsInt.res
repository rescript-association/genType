@genType.import("./bsString")
external packInt: (@int [#a | @as(34) #removeRange | #normal], string, string) => string = "packInt"

let x = packInt(#normal, "a", "b")

