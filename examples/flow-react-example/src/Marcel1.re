module type ModType = {
  type t = {
    name: string,
    surname: string,
  };
  type w;
  type k;
  let foo: t => t;
};

module A = {
  type t = {
    name: string,
    surname: string,
  };
  let foo = x => x;
  type w = int;
  type k = unit;
};

module M = (val (module A): ModType);

[@genType]
type m = M.t;

module N = {
  include M;
};

[@genType]
type n = N.t;

[@genType]
let testConversion = (x: n) => x;

/* Without annotations, type w is never mentioned in the generated code */
type w = N.w;

let n: module ModType = (module N);

module O = (val n: ModType);

[@genType]
type o = O.t;