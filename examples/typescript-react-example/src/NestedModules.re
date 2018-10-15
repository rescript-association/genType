[@genType]
let notNested = 1;

module Universe = {
  [@genType]
  let theAnswer = 42;

  let notExported = 33;

  module Nested2 = {
    let x = 0;

    [@genType]
    let nested2Value = 1;

    let y = 2;

    module Nested3 = {
      let x = 0;
      let y = 1;
      let z = 2;
      let w = 3;

      [@genType]
      let nested3Value = "nested3Value";
    };
  };

  [@genType]
  let someString = "some exported string";
};