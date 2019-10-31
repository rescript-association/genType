[@genType]
let notNested = 1;

module Universe = {
  [@genType]
  let theAnswer = 42;

[@dead "Universe.notExported"]   let notExported = 33;

  [@genType]
  type nestedType = array(string);

  module Nested2 = {
[@dead "Universe.Nested2.x"]     let x = 0;

    [@genType]
    let nested2Value = 1;

[@dead "Universe.Nested2.y"]     let y = 2;

    [@genType]
    type nested2Type = array(array(string));

    module Nested3 = {
[@dead "Universe.Nested2.Nested3.x"]       let x = 0;
[@dead "Universe.Nested2.Nested3.y"]       let y = 1;
[@dead "Universe.Nested2.Nested3.z"]       let z = 2;
[@dead "Universe.Nested2.Nested3.w"]       let w = 3;

      [@genType]
      type nested3Type = array(array(array(string)));

      [@genType]
      let nested3Value = "nested3Value";

      [@genType]
      let nested3Function = (x: nested2Type) => x;
    };

    [@genType]
    let nested2Function = (x: Nested3.nested3Type) => x;
  };

  [@genType]
  type variant =
    | [@dead "Universe.variant.A"] A
    | [@dead "Universe.variant.B"] B(string);

  [@genType]
  let someString = "some exported string";
};