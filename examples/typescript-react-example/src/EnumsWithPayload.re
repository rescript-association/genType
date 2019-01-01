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
type manyPayloads = [ | `one(int) | `two(string, string)];