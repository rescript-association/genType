let component = ReasonReact.statelessComponent("Component");

[@genFlow]
type person = {
  name: string,
  surname: string,
};

[@genFlow]
let make =
    (~message="default message", ~person: person, ~intList=[0], _children)
    : ReasonReact.component(_) => {
  ...component,
  render: _self =>
    <div className="App">
      (
        "ReasonReact "
        ++ message
        ++ " and intList: "
        ++ (
          intList |> List.map(i => string_of_int(i)) |> String.concat(",")
        )
        ++ " and person name: "
        ++ person.name
      )
      ->ReasonReact.string
    </div>,
};

[@genFlow]
let minus = (~first=0, ~second) => first - second;

[@genFlow]
let useTypeDefinedInAnotherModule = (x: Types.t) => x;

[@genFlow]
type t =
  | A
  | B(int)
  | C(string);

[@genFlow]
let tToString = t =>
  switch (t) {
  | A => "A"
  | B(i) => "B(" ++ string_of_int(i) ++ ")"
  | C(s) => "C(" ++ s ++ ")"
  };