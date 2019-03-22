module Nested = {
  let offset = 1
  and shadowed = 3;

  module M = {
    [@genType]
    let a = 34;
  };

  let height = 1
  and shadowed = shadowed + 3;

  let num = 34;

  [@genType]
  let num = num + 1;
};