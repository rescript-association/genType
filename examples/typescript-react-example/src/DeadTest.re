let _ = Js.log(ImmutableArray.fromArray);
let fortytwo = 42;

[@genType]
let fortyTwoButExported = 42;

let thisIsUsedOnce = 34;
ignore(thisIsUsedOnce);

let thisIsUsedTwice = 34;
ignore(thisIsUsedTwice);
ignore(thisIsUsedTwice);

[@dead]
let thisIsMarkedDead = 99;

let thisIsKeptAlive = 42;

[@live]
let thisIsMarkedLive = thisIsKeptAlive;

module Inner = {
  [@dead]
  let thisIsAlsoMarkedDead = 99;
};

module M: {
  [@dead]
  let thisSignatureItemIsDead: int;
} = {
  let thisSignatureItemIsDead = 34;
};

module VariantUsedOnlyInImplementation: {
  type t =
    | A; // TODO: discovered this automatically
  let a: t;
} = {
  type t =
    | A;
  let a = A;
};

let _ = (x => x)(VariantUsedOnlyInImplementation.a);

let _ = DeadTypeTest.OnlyInInterface;
let _ = DeadTypeTest.InBoth;

type record = {
  xxx: int,
  yyy: int,
};

let _ = r => r.xxx;
let _ = ({yyy}) => yyy;

module UnderscoreInside = {
  let _ = 13;
};

module MM: {
  let x: int;
  let y: int;
} = {
  let y = 55;
  let x = {
    y;
  };
  let valueOnlyInImplementation = 7;
};

let _ = {
  Js.log(MM.x);
  44;
};

let () = Js.log(DeadValueTest.valueAlive);

let rec unusedRec = () => unusedRec();

let rec split_map = l => {
  let _ = split_map(l);
  [];
};

let rec rec1 = () => rec2()
and rec2 = () => rec1();

let rec recWithCallback = () => {
  let cb = () => recWithCallback();
  cb();
};

let rec foo = () => {
  let cb = () => bar();
  cb();
}
and bar = () => foo();

let withDefaultValue = (~paramWithDefault=3, y) => paramWithDefault + y;

external unsafe_string1: (bytes, int, int) => Digest.t = "caml_md5_string";

module Ext_buffer: {
  external unsafe_string2: (bytes, int, int) => Digest.t = "caml_md5_string";
} = {
  external unsafe_string2: (bytes, int, int) => Digest.t = "caml_md5_string";
};

let () = Js.log(DeadRT.Root("xzz"));

module LazyExportWithRename = [%lazyLoadComponent ExportWithRename];