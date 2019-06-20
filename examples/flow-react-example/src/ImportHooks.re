[@genType]
type person = {
  name: string,
  age: int,
};

[@genType.import "./hookExample"] [@react.component]
external make:
  (
    ~person: person,
    ~children: React.element,
    ~renderMe: {. "randomString": string} => React.element
  ) =>
  React.element =
  "";

[@genType.import "./hookExample"]
external foo: (~person: person) => string = "";