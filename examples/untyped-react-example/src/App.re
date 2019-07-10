let component = ReasonReact.statelessComponent("App");

type person = {
  name: string,
  age: int,
  optional: option(float),
  unknown: option(list(int)),
};

[@genType]
let make = (~array, ~callback=() => (), ~person, ~title, _children) => {
  ...component,
  render: _self => {
    callback();
    <div>
      {ReasonReact.string(
         "Test Component Title:"
         ++ title
         ++ " Name:"
         ++ person.name
         ++ " array[0]:"
         ++ array[0],
       )}
    </div>;
  },
};

[@genType]
let poly = (x, _y) => x + 1;