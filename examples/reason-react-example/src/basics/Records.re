open Belt;

[@genFlow]
type coord = {
  x: int,
  y: int,
  z: option(int),
};

[@genFlow]
let origin = {x: 0, y: 0, z: Some(0)};

[@genFlow]
let computeArea = ({x, y, z}) =>
  Option.(x * y * z->(mapWithDefault(1, n => n)));

[@genFlow]
let coord2d = (x, y) => {x, y, z: None};