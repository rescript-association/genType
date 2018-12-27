
type payload = {
  x: int,
  y: option(string),
};

type withPayload = [
  | `a
  | [@genType.as "bRenamed"] `b
  | [@genType.as true] `True
  | [@genType.as 20] `Twenty
  | `c(payload)
];

[@genType]
let testWithPayload = (x: withPayload) => x;

let a = `a;

let c = `c({x: 3, y: None});