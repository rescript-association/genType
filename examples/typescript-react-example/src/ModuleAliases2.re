[@genType]
type record = {
  [@dead "record.x"] x: int,
  [@dead "record.y"] y: string,
};

module Outer = {
  [@genType]
  type outer = {[@dead "Outer.outer.outer"] outer: string};

  module Inner = {
    [@genType]
    type inner = {[@dead "Outer.Inner.inner.inner"] inner: string};
  };
};

module OuterAlias = Outer;

module InnerAlias = OuterAlias.Inner;

[@dead "q"] let q = 42;