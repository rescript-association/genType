[@genType]
type person = {
  name: string,
  age: int,
};

[@genType.import "./hookExample"] [@react.component]
external make: (~person: person, ~children: React.element) => React.element =
  "";

let make = make;

[@genType.import "./hookExample"]
external foo: (~person: person) => string = "";