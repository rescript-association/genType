[@genType]
type t = int;

[@genType]
let someIntList = [1, 2, 3];

[@genType]
let map = List.map;

[@genType]
type typeWithVars('x, 'y, 'z) =
  | A('x, 'y)
  | B('z);

[@genType]
type tree = {
  .
  "label": string,
  "left": option(tree),
  "right": option(tree),
};

/*
 * A tree is a recursive type which does not require any conversion (JS object).
 * All is well.
 */
[@genType]
let rec swap = (tree: tree): tree => {
  "label": tree##label,
  "left": tree##right->(Belt.Option.map(swap)),
  "right": tree##left->(Belt.Option.map(swap)),
};

[@genType]
type selfRecursive = {self: selfRecursive};

[@genType]
type mutuallyRecursiveA = {b: mutuallyRecursiveB}
and mutuallyRecursiveB = {a: mutuallyRecursiveA};

/*
 * This is a recursive type which requires conversion (a record).
 * Only a shallow conversion of the top-level element is performed.
 */
[@genType]
let selfRecursiveConverter = ({self}) => self;

/*
 * This is a mutually recursive type which requires conversion (a record).
 * Only a shallow conversion of the two top-level elements is performed.
 */
[@genType]
let mutuallyRecursiveConverter = ({b}) => b;

[@genType]
let testFunctionOnOptionsAsArgument = (a: option('a), foo) => foo(a);

[@genType.opaque]
type opaqueVariant =
  | A
  | B;

[@genType]
let stringT: String.t = "a";

[@genType]
let jsStringT: Js.String.t = "a";

[@genType]
type twice('a) = ('a, 'a);

[@gentype]
type genTypeMispelled = int;

[@genType]
type dictString = Js.Dict.t(string);

[@genType]
let jsonStringify = Js.Json.stringify;

[@genType]
type nullOrString = Js.Null.t(string);

[@genType]
type nullOrString2 = Js.null(string);

type record = {
  i: int,
  s: string,
};

[@genType]
let testConvertNull = (x: Js.Null.t(record)) => x;

[@genType]
type decorator('a, 'b) = 'a => 'b constraint 'a = int constraint 'b = _ => _;

[@genType]
let testConvertLocation = (x: Location.t) => x;

/* Bucklescript's marshaling rules. */
[@genType]
type marshalFields = {
  .
  "_rec": string,
  "_switch": string,
  "switch__": string,
  "_": string,
  "__": string,
  "___": string,
  "foo__": string,
  "_foo__": string,
  "_Uppercase": string,
  "_Uppercase__": string,
};

[@genType]
let testMarshalFields: marshalFields = {
  "_rec": "rec",
  "_switch": "_switch", /* reason keywords are not recognized */
  "switch__": "switch",
  "_": "_",
  "__": "__",
  "___": "_",
  "foo__": "foo",
  "_foo__": "_foo",
  "_Uppercase": "Uppercase",
  "_Uppercase__": "_Uppercase",
};

[@genType]
type marshalMutableField = {. [@bs.set] "_match": int};

[@genType]
let setMatch = (x: marshalMutableField) => x##_match #= 34;