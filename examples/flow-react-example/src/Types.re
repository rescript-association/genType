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

[@bs.module "./SomeFlowTypes"] external saturday: weekday = "SATURDAY";
[@bs.module "./SomeFlowTypes"] external sunday: weekday = "SUNDAY";
[@bs.module "./SomeFlowTypes"] external monday: weekday = "MONDAY";

[@genType]
let isWeekend = day => day === saturday || day === sunday;

[@genType]
let testFunctionOnOptionsAsArgument = (a: option('a), foo) => foo(a);

[@genType]
type someMutableFields = {
  .
  [@bs.set] "mutable0": string,
  "immutable": int,
  [@bs.set] "mutable1": string,
  [@bs.set] "mutable2": string,
};

[@genType.import "./name-with-dashes"] external foo: int => int = "foo";

[@genType.opaque]
type exportOpaqueFromVariants = Variants.weekday;

[@genType]
[@genType.as "DateKey"]
type dateKey = string;

[@genType.opaque]
[@genType.as "DateKeyOpaque"]
type dateKeyOpaque = string;

[@genType]
let testDateKey = (x: dateKey) => x;

[@genType]
let testAutoAnnotateVariants = (x: AutoAnnotate.variant) => x;

[@genType]
let testAutoAnnotateVariants2 = (x: AutoAnnotate.annotatedVariant) => x;

[@genType.opaque]
type opaqueVariant =
  | A
  | B;

[@genType]
[@genType.as "Filter"]
type filter = {
  name: string,
  values: array(string),
};

[@genType]
type twice('a) = ('a, 'a);

[@genType]
type gadt =
  | F: gadt;

type objectWithCallback = {
  .
  "y": option({. "z": option(unit => int)}),
  "x": option(unit => int),
};

[@genType]
let convertObjectWithCallback = (x: objectWithCallback) => x;

type ocaml_array('a) = array('a);

// This should be considered annotated automatically.
type someRecord = {id: int};

type instantiateTypeParameter = ocaml_array(someRecord);

[@genType]
let testInstantiateTypeParameter = (x: instantiateTypeParameter) => x;

[@genType]
type date = Js.Date.t;

[@genType]
let currentTime = Js.Date.make();

[@genType]
let optFunction = Some(() => 3);