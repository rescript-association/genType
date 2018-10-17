[@genType]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genType]
type optionInt = option(int);

[@genType]
let consumeOption = (x: option(int)) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));

[@genType]
let consumeOption2 = (x: optionInt) =>
  Belt.Option.(x->(mapWithDefault(0, n => n)));

[@genType]
let testArray = (a: array(option(int))): array(option(int)) => a;

[@genType]
type funType = int => int;

[@genType]
type myFloat = float;

[@genType]
type arrayOfStrings1 = array(string);

[@genType]
type arrayOfStrings2 = Js.Array.t(string);

[@genType]
type maybeString = Js.null_undefined(string);

[@genType]
type maybeString2 = Js.Null_undefined.t(string);

[@genType]
type peopleArray =
  array({
    .
    "name": string,
    "nickname": Js.nullable(string),
  });

[@genType]
type myObj = Obj.t;

/* Defines a type which maps to `anInterestingFlowType` in `SomeFlowTypes.js` */
[@genType.import "./SomeFlowTypes"]
type anInterestingFlowType;

[@genType]
let identity = (x: anInterestingFlowType) => x;

[@genType.import "./SomeFlowTypes"]
type weekday;

[@genType]
type t = weekday;

[@bs.module "./SomeFlowTypes"] external saturday: t = "SATURDAY";
[@bs.module "./SomeFlowTypes"] external sunday: t = "SUNDAY";
[@bs.module "./SomeFlowTypes"] external monday: t = "MONDAY";

[@genType]
let isWeekend = day => day === saturday || day === sunday;

module Poly = {
  [@genType]
  type weekday = [ | `monday | `saturday | `sunday];

  [@genType]
  let isWeekend = x =>
    switch (x) {
    | `saturday
    | `sunday => true
    };

  [@genType]
  let monday = `monday;
  [@genType]
  let saturday = `saturday;
  [@genType]
  let sunday = `sunday;

  [@genType]
  let onlySunday = (_: [ | `sunday]) => ();
};