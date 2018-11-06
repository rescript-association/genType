/**
  * Wrap JS values to be used from Reason
  */
[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.gt"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external round: float => float = "";

[@genType]
type point = {
  x: int,
  y: option(int),
};

[@genType.import "./MyMath"] /* This is the module to import from. */
[@bs.module "./WrapJsValue.gt"] /* This must always be the name of the current module. */
/* Name and type of the JS value to bind to. */
external area: point => int = "";

[@genType]
let roundedNumber = round(1.8);

[@genType]
let areaValue = area({x: 3, y: None});

module AbsoluteValue = {
  [@genType.import "./MyMath"]
  [@genType.as "AbsoluteValue"]
  type t = {. "getAbs": (. unit) => int};

  /* This is untyped */
  [@bs.send] external getProp: t => int = "getProp";

  /* This is also untyped, as we "trust" the type declaration in absoluteVaue */
  let getAbs = (x: t) => {
    let getAbs = x##getAbs;
    getAbs(.);
  };
};

[@genType]
let useGetProp = (x: AbsoluteValue.t) => x->AbsoluteValue.getProp + 1;

[@genType]
let useGetAbs = (x: AbsoluteValue.t) => x->AbsoluteValue.getAbs + 1;

[@genType.import "./MyMath"]
type stringFunction;