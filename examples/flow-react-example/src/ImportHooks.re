[@genType]
type person = {
  name: string,
  age: int,
};

[@genType]
type renderMe('a) =
  {
    .
    "randomString": string,
    "poly": 'a,
  } =>
  React.element;

[@genType.import "./hookExample"] [@react.component]
external make:
  (~person: person, ~children: React.element, ~renderMe: renderMe('a)) =>
  React.element =
  "makeRenamed";

[@genType.import "./hookExample"]
external foo: (~person: person) => string = "foo";