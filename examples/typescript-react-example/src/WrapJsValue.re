/**
  * Wrap JS values to be used from Reason
  */
[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external round: float => float = "";

[@genType]
type point = {
  x: int,
  y: option(int),
};

[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external area: point => int = "";

[@genType]
let roundedNumber = round(1.8);

[@genType]
let areaValue = area({x: 3, y: None});

[@genType.import "./MyMath"]
[@genType.as "AbsoluteValue"]
type absoluteVaue = {. "getAbs": (. unit) => int};

/* This is untyped */
[@bs.send] external getProp: absoluteVaue => int = "getProp";

/* This is also untyped, as we "trust" the type declaration in absoluteVaue */
let getAbs = (x: absoluteVaue) => {
  let getAbs = x##getAbs;
  getAbs(.);
};

[@genType]
let useGetProp = (x: absoluteVaue) => x->getProp + 1;

[@genType]
let useGetAbs = (x: absoluteVaue) => x->getAbs + 1;