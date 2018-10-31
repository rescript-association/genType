/**
  * Wrap JS values to be used from Reason
  */
[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.re"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external round: float => float = "";

[@genType]
type point = {
  x: int,
  y: option(int),
};

[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.re"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external area: point => int = "";

[@genType]
let roundedNumber = round(1.8);

[@genType]
let areaValue = area({x: 3, y: None});

[@genType.import "./MyMath"] /* This is the module to import from. */
type myArray('a);

[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.re"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external getValueAtIndex: (myArray(string), int) => string = "";