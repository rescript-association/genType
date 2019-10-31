type payload = {
  x: int,
  y: option(string),
};

type withPayload = [
  | `a
  | [@genType.as "bRenamed"] `b
  | [@genType.as true] `True
  | [@genType.as 20] `Twenty
  | [@genType.as 0.5] `Half
  | `c(payload)
];

[@genType]
let testWithPayload = (x: withPayload) => x;

[@genType]
let printVariantWithPayload = (x: withPayload) =>
  switch (x) {
  | `a => Js.log("printVariantWithPayload: a")
  | `b => Js.log("printVariantWithPayload: b")
  | `True => Js.log("printVariantWithPayload: True")
  | `Twenty => Js.log("printVariantWithPayload: Twenty")
  | `Half => Js.log("printVariantWithPayload: Half")
  | `c(payload) =>
    Js.log4("printVariantWithPayload x:", payload.x, "y:", payload.y)
  };

[@genType]
type manyPayloads = [
  | [@genType.as "oneRenamed"] `one(int)
  | [@genType.as 2] `two(string, string)
  | `three(payload)
];

[@genType]
let testManyPayloads = (x: manyPayloads) => x;

[@genType]
let printManyPayloads = (x: manyPayloads) =>
  switch (x) {
  | `one(n) => Js.log2("printManyPayloads one:", n)
  | `two(s1, s2) => Js.log3("printManyPayloads two:", s1, s2)
  | `three(payload) =>
    Js.log4("printManyPayloads x:", payload.x, "y:", payload.y)
  };

[@genType]
type simpleVariant =
  | [@dead "simpleVariant.A"] A
  | [@dead "simpleVariant.B"] B
  | [@dead "simpleVariant.C"] C;

[@genType]
let testSimpleVariant = (x: simpleVariant) => x;

[@genType]
type variantWithPayloads =
  | [@dead "variantWithPayloads.A"] [@genType.as "ARenamed"] A
  | [@dead "variantWithPayloads.B"] B(int)
  | [@dead "variantWithPayloads.C"] C(int, int)
  | [@dead "variantWithPayloads.D"] D((int, int))
  | [@dead "variantWithPayloads.E"] E(int, string, int);

[@genType]
let testVariantWithPayloads = (x: variantWithPayloads) => x;

[@genType]
let printVariantWithPayloads = x =>
  switch (x) {
  | A => Js.log2("printVariantWithPayloads", "A")
  | B(x) =>
    Js.log2("printVariantWithPayloads", "B(" ++ string_of_int(x) ++ ")")
  | C(x, y) =>
    Js.log2(
      "printVariantWithPayloads",
      "C(" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")",
    )
  | D((x, y)) =>
    Js.log2(
      "printVariantWithPayloads",
      "D((" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ "))",
    )
  | E(x, s, y) =>
    Js.log2(
      "printVariantWithPayloads",
      "E("
      ++ string_of_int(x)
      ++ ", "
      ++ s
      ++ ", "
      ++ string_of_int(y)
      ++ ")",
    )
  };

[@genType]
type variant1Int =
  | [@dead "variant1Int.R"] R(int);

[@genType]
let testVariant1Int = (x: variant1Int) => x;

[@genType]
type variant1Object =
  | [@dead "variant1Object.R"] R(payload);

[@genType]
let testVariant1Object = (x: variant1Object) => x;