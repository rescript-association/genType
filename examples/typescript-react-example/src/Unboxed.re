[@genType]
[@ocaml.unboxed]
type v1 =
  | A(int);

[@unboxed]
[@genType]
type v2 =
  | A(int);

[@genType]
let testV1 = (x: v1) => x;