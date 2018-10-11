[@genType.import "./MyMath"] [@bs.module "./Test.re"]
external round: float => float = "";

[@genType]
let roundedNumber = round(1.8);

Js.log2("Test.re roundedNumber:", roundedNumber);