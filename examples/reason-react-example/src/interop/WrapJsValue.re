/* This is the module to import from. */
[@genType.import "./MyMath"]
/* This must always be the name of the current module. */
[@bs.module "./WrapJsValue.re"]
/* This is the name and type of the JS value to bind to. */
external round: float => float = "";

[@genType]
type point = {
  x: int,
  y: option(int),
};

/* This is the module to import from. */
[@genType.import "./MyMath"]
/* This must always be the name of the current module. */
[@bs.module "./WrapJsValue.re"]
/* This is the name and type of the JS value to bind to. */
external area: point => int = "";

[@genType]
let roundedNumber = round(1.8);

[@genType]
let areaValue = area({x: 3, y: None});