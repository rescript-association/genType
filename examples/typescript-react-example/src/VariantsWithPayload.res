type payload = {
  x: int,
  y: option<string>,
}

type withPayload = [
| #a
| @genType.as("bRenamed")
#b
| @genType.as(true)
#True
| @genType.as(20)
#Twenty
| @genType.as(0.5)
#Half
| #c(payload)
]

export testWithPayload = (x: withPayload) => x

export printVariantWithPayload = (x: withPayload) =>
  switch x {
  | #a => Js.log("printVariantWithPayload: a")
  | #b => Js.log("printVariantWithPayload: b")
  | #True => Js.log("printVariantWithPayload: True")
  | #Twenty => Js.log("printVariantWithPayload: Twenty")
  | #Half => Js.log("printVariantWithPayload: Half")
  | #c(payload) => Js.log4("printVariantWithPayload x:", payload.x, "y:", payload.y)
  }

export type manyPayloads = [
| @genType.as("oneRenamed") #one(int)
| @genType.as(2) #two(string, string)
| #three(payload)
]

export testManyPayloads = (x: manyPayloads) => x

export printManyPayloads = (x: manyPayloads) =>
  switch x {
  | #one(n) => Js.log2("printManyPayloads one:", n)
  | #two(s1, s2) => Js.log3("printManyPayloads two:", s1, s2)
  | #three(payload) => Js.log4("printManyPayloads x:", payload.x, "y:", payload.y)
  }

export type simpleVariant =
  | A
  | B
  | C

export testSimpleVariant = (x: simpleVariant) => x

export type variantWithPayloads =
  | @genType.as("ARenamed") A
  | B(int)
  | C(int, int)
  | D((int, int))
  | E(int, string, int)

export testVariantWithPayloads = (x: variantWithPayloads) => x

export printVariantWithPayloads = x =>
  switch x {
  | A => Js.log2("printVariantWithPayloads", "A")
  | B(x) => Js.log2("printVariantWithPayloads", "B(" ++ (string_of_int(x) ++ ")"))
  | C(x, y) =>
    Js.log2(
      "printVariantWithPayloads",
      "C(" ++ (string_of_int(x) ++ (", " ++ (string_of_int(y) ++ ")"))),
    )
  | D((x, y)) =>
    Js.log2(
      "printVariantWithPayloads",
      "D((" ++ (string_of_int(x) ++ (", " ++ (string_of_int(y) ++ "))"))),
    )
  | E(x, s, y) =>
    Js.log2(
      "printVariantWithPayloads",
      "E(" ++ (string_of_int(x) ++ (", " ++ (s ++ (", " ++ (string_of_int(y) ++ ")"))))),
    )
  }

export type variant1Int = R(int)

export testVariant1Int = (x: variant1Int) => x

export type variant1Object = R(payload)

export testVariant1Object = (x: variant1Object) => x
