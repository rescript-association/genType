export type t = int

export someIntList = list{1, 2, 3}

export map = List.map

export type typeWithVars<'x, 'y, 'z> =
  | A('x, 'y)
  | B('z)

export type rec tree = {"label": string, "left": option<tree>, "right": option<tree>}

/*
 * A tree is a recursive type which does not require any conversion (JS object).
 * All is well.
 */
export rec swap = (tree: tree): tree =>
  {
    "label": tree["label"],
    "left": tree["right"]->Belt.Option.map(swap),
    "right": tree["left"]->Belt.Option.map(swap),
  }

export type rec selfRecursive = {self: selfRecursive}

export type rec mutuallyRecursiveA = {b: mutuallyRecursiveB}
and mutuallyRecursiveB = {a: mutuallyRecursiveA}

/*
 * This is a recursive type which requires conversion (a record).
 * Only a shallow conversion of the top-level element is performed.
 */
export selfRecursiveConverter = ({self}) => self

/*
 * This is a mutually recursive type which requires conversion (a record).
 * Only a shallow conversion of the two top-level elements is performed.
 */
export mutuallyRecursiveConverter = ({b}) => b

export testFunctionOnOptionsAsArgument = (a: option<'a>, foo) => foo(a)

@genType.opaque
type opaqueVariant =
  | A
  | B

export stringT: String.t = "a"

export jsStringT: Js.String.t = "a"

export jsString2T: Js.String2.t = "a"

export type twice<'a> = ('a, 'a)

@gentype
type genTypeMispelled = int

export type dictString = Js.Dict.t<string>

export jsonStringify = Js.Json.stringify

export type nullOrString = Js.Null.t<string>

export type nullOrString2 = Js.null<string>

type record = {
  i: int,
  s: string,
}

export testConvertNull = (x: Js.Null.t<record>) => x

export type decorator<'a, 'b> = 'a => 'b constraint 'a = int constraint 'b = _ => _

export testConvertLocation = (x: Location.t) => x

/* Bucklescript's marshaling rules. */
export type marshalFields = {
  "_rec": string,
  "_switch": string,
  "switch": string,
  "__": string,
  "___": string,
  "foo__": string,
  "_foo__": string,
  "_Uppercase": string,
  "_Uppercase__": string,
}

export testMarshalFields: marshalFields = {
  "_rec": "rec",
  "_switch" /* reason keywords are not recognized */: "_switch",
  "switch": "switch",
  "__": "__",
  "___": "_",
  "foo__": "foo",
  "_foo__": "_foo",
  "_Uppercase": "Uppercase",
  "_Uppercase__": "_Uppercase",
}

export type marshalMutableField = {@bs.set "_match": int}

export setMatch = (x: marshalMutableField) => x["_match"] = 34

type ocaml_array<'a> = array<'a>

// This should be considered annotated automatically.
type someRecord = {id: int}

type instantiateTypeParameter = ocaml_array<someRecord>

export testInstantiateTypeParameter = (x: instantiateTypeParameter) => x

@genType.as("Vector")
export type vector<'a> = ('a, 'a)

export type date = Js.Date.t

export currentTime = Js.Date.make()

export type i64A = Int64.t

export type i64B = int64

export i64Const: i64B = 34L

export optFunction = Some(() => 3)

module ObjectId: {
  export type t = int
} = {
  type t = int
  let x = 1
}
