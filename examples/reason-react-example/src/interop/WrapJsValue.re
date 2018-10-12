[@genType.import "./MyMath"] [@bs.module "./WrapJsValue.re"]
external round: float => float = "";

[@genType]
type point = {
  x: int,
  y: option(int),
};

[@genType.import "./MyMath"] [@bs.module "./WrapJsValue.re"]
external area: point => int = "";

[@genType]
let roundedNumber = round(1.8);

Js.log2("Test.re roundedNumber:", roundedNumber);

[@genType]
let areaValue = area({x: 3, y: None});

Js.log2("Test.re areaValue:", areaValue);