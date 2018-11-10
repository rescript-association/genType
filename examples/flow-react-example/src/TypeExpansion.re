[@genType]
type personFromTuples = Tuples.person;

/* This type references module Tuple -- an import must be generated when this is expanded */
type lowerType = {person: Tuples.person};

type middleType = {. "lowerType": lowerType};

/* This is the only annotated type. Type expansion. */
[@genType]
type topType = (int, middleType);

[@genType]
let testConversion = (x: topType) => x;

/* MarcelCutt's example */
module A = {
  type user = {name: string};
};

[@genType]
type b = A.user;