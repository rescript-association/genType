type vehicle = {name: string};

[@react.component]
let make = (~vehicle) => {
  let (count, setCount) = React.useState(() => 0);

  <div>
    <p>
      {React.string(
         "Hooks example "
         ++ vehicle.name
         ++ " clicked "
         ++ string_of_int(count)
         ++ " times",
       )}
    </p>
    <button onClick={_ => setCount(_ => count + 1)}>
      {React.string("Click me")}
    </button>
    <ImportHooks person={name: "Mary", age: 71}>
      {React.string("child1")}
      {React.string("child2")}
    </ImportHooks>
  </div>;
};

[@genType]
let default = make;

[@genType]
[@react.component]
let anotherComponent = (~vehicle) =>
  <div> {React.string("Another Hook " ++ vehicle.name)} </div>;

module Inner = {
  [@genType]
  [@react.component]
  let make = (~vehicle) =>
    <div> {React.string("Another Hook " ++ vehicle.name)} </div>;

  [@genType]
  [@react.component]
  let anotherComponent = (~vehicle) =>
    <div> {React.string("Another Hook " ++ vehicle.name)} </div>;

  module Inner2 = {
    [@genType]
    [@react.component]
    let make = (~vehicle) =>
      <div> {React.string("Another Hook " ++ vehicle.name)} </div>;

    [@genType]
    [@react.component]
    let anotherComponent = (~vehicle) =>
      <div> {React.string("Another Hook " ++ vehicle.name)} </div>;
  };
};

[@genType]
[@react.component]
let makeWithRef = (~vehicle, ref) => {
  switch (ref->Js.Nullable.toOption) {
  | Some(ref) => ref->React.Ref.setCurrent(10)
  | None => ()
  };
  React.string(vehicle.name);
};

[@genType]
let testForwardRef = React.forwardRef(makeWithRef);