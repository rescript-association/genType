[@genType]
type person = {
  [@dead "person.name"] name: string,
  [@dead "person.age"] age: int,
};

[@genType]
type renderMe('a) =
  React.component({
    .
    "randomString": string,
    "poly": 'a,
  });

[@genType.import "./hookExample"] [@react.component]
external make:
  (~person: person, ~children: React.element, ~renderMe: renderMe('a)) =>
  React.element =
  "makeRenamed";

[@genType.import "./hookExample"]
external foo: (~person: person) => string = "foo";