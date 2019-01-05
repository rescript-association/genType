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
let printEnumValue = (x: withPayload) =>
  switch (x) {
  | `a => Js.log("printEnumValue: a")
  | `b => Js.log("printEnumValue: b")
  | `True => Js.log("printEnumValue: True")
  | `Twenty => Js.log("printEnumValue: Twenty")
  | `Half => Js.log("printEnumValue: Half")
  | `c(payload) => Js.log4("printEnumValue x:", payload.x, "y:", payload.y)
  };

[@genType]
type manyPayloads = [ | `one(int) | `two(string, string) | `three(payload)];

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
  | A
  | B
  | C;

[@genType]
let testSimpleVariant = (x: simpleVariant) => x;

[@genType]
type variantWithPayloads =
  | A
  | B(int)
  | C(int, int)
  | D((int, int))
  | E(int, string, int);

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

/*
 [@genType]
 type variant1 =
   | R(int);

 [@genType]
 let testVariant1 = (x: variant1) => x;

 [@genType]
 type polyVariant1 = [ | `R(int)];

 [@genType]
 let testPolyVariant1 = (x: polyVariant1) => x;

 [@genType]
 type variant2 =
   | R(int)
   | S(int);

 [@genType]
 let testVariant2 = (x: variant2) => x;

 [@genType]
 type polyVariant2 = [ | `R(int) | `S(int)];

 [@genType]
 let testPolyVariant2 = (x: polyVariant2) => x;
 */