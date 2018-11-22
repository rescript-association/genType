type variant =
  | R(int);

[@genType]
type record = {variant};

type r2 = {r2: int};

type r3 = {r3: int};

type r4 = {r4: int};

[@genType]
type annotatedVariant =
  | R2(r2, r3)
  | R4(r4);

type r5 = {r5: int};

[@genType]
let useR5 = (x: r5) => x;

type r6 = {r6: int};

let component = ReasonReact.statelessComponent(__MODULE__);

[@genType]
let make = (~r6 as _: r6, _children) => {
  ...component,
  render: _ => ReasonReact.null,
};