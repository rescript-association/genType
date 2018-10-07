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
let computeNestedNestedHalfNullable =
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
              option({
                .
                "x": int,
                "y": int,
                "z": Js.Nullable.t(int),
              }),
          }),
      },
    )
    : int => 0;