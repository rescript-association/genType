[@genFlow]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genFlow]
type coord = {
  x: int,
  y: int,
  z: option(int),
};

[@genFlow]
type optionInt = option(int);

[@genFlow]
let consumeOption = (x: option(int)) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));

[@genFlow]
let consumeOption2 = (x: optionInt) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));