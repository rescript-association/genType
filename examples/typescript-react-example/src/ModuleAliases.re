module Outer = {
  module Inner = {
    type innerT = {inner: string};
  };
};

module Outer2 = {
  module OuterInnerAlias = Outer.Inner;
  module Inner2 = {
    module InnerNested = {
      type t = {nested: int};
    };
    module OuterInnerAlias2 = OuterInnerAlias;
  };
};

module Outer2Alias = Outer2;

module InnerNestedAlias = Outer2.Inner2.InnerNested;

[@genType]
let testNested = (x: InnerNestedAlias.t) => x;

[@genType]
let testInner = (x: Outer2Alias.OuterInnerAlias.innerT) => x;

[@genType]
let testInner2 = (x: Outer2Alias.Inner2.OuterInnerAlias2.innerT) => x;

/*
 module MyBB = BB;

 [@genType]
 type bbRecord = BB.record;

 [@genType]
 type myBBRecord = MyBB.record;

 module MyBin = BB.Bin;

 [@genType]
 type myBBin = MyBB.Bin.bin;

 [@genType]
 type myBBin2 = MyBin.bin;

 module BB1 = BB;
 module BB2 = BB1.Bin;
 module BB3 = BB2.Bin2;

 [@genType]
 type my2 = BB3.in2;
 */
/*
  [@genType]
  type my2alias = BB.Bin2Alias.in2;

  [@genType]
  type my2aliasXX = BB.Bin.Bin2.in2;
 */
/*
 [@genType]
 let gaaAlias = (x: BB.Bin2Alias.in2) => x;

 [@genType]
 let gaaDirect = (x: BB.Bin.Bin2.in2) => x;

 */