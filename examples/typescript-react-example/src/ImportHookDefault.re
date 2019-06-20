type person = {
  name: string,
  age: int,
};

[@genType.import ("./hookExample", "default")] [@react.component]
external make:
  (
    ~person: person,
    ~children: React.element,
    ~renderMe: {. "randomString": string} => React.element
  ) =>
  React.element =
  "";