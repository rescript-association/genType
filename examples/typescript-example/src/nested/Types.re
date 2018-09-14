[@genFlow]
type t = int;

[@genFlow]
let someIntList = [1, 2, 3];

[@genFlow]
let map = List.map;

[@genFlow]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);