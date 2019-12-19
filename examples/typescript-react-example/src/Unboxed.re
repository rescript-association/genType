[@genType]
[@ocaml.unboxed]
type v1 =
  | A(int);

[@genType]
[@unboxed]
type v2 =
  | A(int);

[@genType]
let testV1 = (x: v1) => x;

[@genType]
[@unboxed]
type r = {x: int};