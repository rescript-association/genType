[@genFlow]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genFlow]
type coord = {
  x: int,
  y: int,
};