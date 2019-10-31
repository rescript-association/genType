type person = {
  [@dead "person.name"] name: string,
  [@dead "person.age"] age: int,
};

[@genType.import ("./hookExample", "default")] [@react.component]
external make:
  (
    ~person: person,
    ~children: React.element,
    ~renderMe: ImportHooks.renderMe(string)
  ) =>
  React.element =
  "make";

[@genType.import "./hookExample"] [@react.component]
external make2:
  (
    ~person: person,
    ~children: React.element,
    ~renderMe: ImportHooks.renderMe(string)
  ) =>
  React.element =
  "default";