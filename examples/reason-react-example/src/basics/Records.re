open Belt;

[@genType]
type coord = {
  x: int,
  y: int,
  z: option(int),
};

[@genType]
let origin = {x: 0, y: 0, z: Some(0)};

[@genType]
let computeArea = ({x, y, z}) =>
  Option.(x * y * z->(mapWithDefault(1, n => n)));

[@genType]
let coord2d = (x, y) => {x, y, z: None};

[@genType]
type coord2 = {
  a: int,
  b: int,
  c: Js.Nullable.t(int),
};

[@genType]
let computeArea2 = ({a, b, c}) =>
  a * b * c->Js.Nullable.toOption->Option.mapWithDefault(1, n => n);