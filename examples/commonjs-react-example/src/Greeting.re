/* ReasonReact used by ReactJS */
/* This is just a normal stateless component. The only change you need to turn
   it into a ReactJS-compatible component is the wrapReasonForJs call below */
let component = ReasonReact.statelessComponent("PageReason");

[@genType]
let onClick = (_: ReactEvent.Mouse.t): unit => Js.log("click");

[@genType]
let make = (~message, ~someNumber, ~extraGreeting=?, _children) => {
  ...component,
  render: _self => {
    let greeting =
      switch (extraGreeting) {
      | None => "How are you?"
      | Some(g) => g
      };
    <div onClick>
      <ImportMyBanner
        show=true
        message={Js.Nullable.return(message ++ " " ++ greeting)}
      />
      {ReasonReact.string("someNumber:" ++ string_of_int(someNumber))}
    </div>;
  },
};

[@genType]
let testBike = (x: Bike.kind) => x;