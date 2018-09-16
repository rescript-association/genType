[@genType]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genType]
type optionInt = option(int);

[@genType]
let consumeOption = (x: option(int)) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));

[@genType]
let consumeOption2 = (x: optionInt) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));

[@genType]
let testArray = (a: array(option(int))): array(option(int)) => a;