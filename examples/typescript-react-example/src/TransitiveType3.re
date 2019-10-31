[@genType]
type t3 = {
  [@dead "t3.i"] i: int,
  [@dead "t3.s"] s: string,
};

[@genType]
let convertT3 = (x: t3) => x;