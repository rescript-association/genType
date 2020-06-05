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

[@genType]
let computeArea3 =
    (
      o: {
        .
        "x": int,
        "y": int,
        "z": Js.Nullable.t(int),
      },
    ) =>
  o##x * o##y * o##z->Js.Nullable.toOption->Option.mapWithDefault(1, n => n);

[@genType]
let computeArea4 =
    (
      o: {
        .
        "x": int,
        "y": int,
        "z": option(int),
      },
    ) =>
  o##x * o##y * o##z->Option.mapWithDefault(1, n => n);

[@genType]
let computeNested =
    (
      _: {
        .
        "x": int,
        "y": int,
        "z":
          option({
            .
            "x": int,
            "y": int,
            "z": option(int),
          }),
      },
    )
    : int => 0;

[@genType]
let computeNestedNested =
    (
      _: {
        .
        "x": int,
        "y": int,
        "z":
          option({
            .
            "x": int,
            "y": int,
            "z":
              option({
                .
                "x": int,
                "y": int,
                "z": option(int),
              }),
          }),
      },
    )
    : int => 0;

[@genType]
let computeNestedNestedNullable =
    (
      _: {
        .
        "x": int,
        "y": int,
        "z":
          Js.Nullable.t({
            .
            "x": int,
            "y": int,
            "z":
              Js.Nullable.t({
                .
                "x": int,
                "y": int,
                "z": Js.Nullable.t(int),
              }),
          }),
      },
    )
    : int => 0;

[@genType]
type bigType = {
  .
  "x": int,
  "y": int,
  "z":
    Js.nullable({
      .
      "x": int,
      "y": int,
      "z":
        option({
          .
          "x": int,
          "y": int,
          "z": Js.nullable(int),
        }),
    }),
};

[@genType]
let computeNestedNestedHalfNullable = (_: bigType): int => 0;

[@genType]
type testMutable = {
  mutable mutableField: int,
  immutableField: int,
};

[@genType]
let useTypeImportedInOtherModule = (x: Types.weekday) => x;

[@genType.opaque]
type innerRecord = {inner: int};

[@genType]
type outerRecord = {innerRecord};

[@genType]
let convertInner = (x: innerRecord) => x;

[@genType]
let convertOuter = (x: outerRecord) => x;

[@genType]
type myRecBsAs = {
  [@bs.as "jsValid0"]
  valid: string,
  [@bs.as "type"]
  type_: string,
  [@bs.as "the-key"]
  theKey: string,
  [@bs.as "with\"dquote"]
  withDQuote: string,
  [@bs.as "with'squote"]
  withSQuote: string,
  [@bs.as "1number"]
  number1: string,
};

[@genType]
let testMyRecBsAs = (x: myRecBsAs) => [|
  x.valid,
  x.type_,
  x.theKey,
  x.withDQuote,
  x.withSQuote,
  x.number1,
|];

[@genType]
let testMyRecBsAs2 = (x: myRecBsAs) => x;
