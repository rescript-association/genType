/* ReasonReact used by ReactJS */
/* This is just a normal stateless component. The only change you need to turn
   it into a ReactJS-compatible component is the wrapReasonForJs call below */
let component = ReasonReact.statelessComponent("PageReason");

[@genFlow]
let make = (~message, ~someNumber, ~extraGreeting=?, _children) => {
  ...component,
  render: _self => {
    let greeting =
      switch (extraGreeting) {
      | None => "How are you?"
      | Some(g) => g
      };
    <div>
      <MyBannerRe show=true message=(message ++ " " ++ greeting) />
      (ReasonReact.string("someNumber:" ++ string_of_int(someNumber)))
    </div>;
  },
};

[@genFlow]
let empty: list(string) = [];

[@genFlow]
let cons = (~x, ~l) => [x, ...l];

[@genFlow]
let cons2 = (~l, ~x) => [x, ...l];

[@genFlow]
let concat = String.concat;

[@genFlow]
let testNamedArgs = (~a, ~b, x, ~c, ~d, y, ~e) => a + b + x + c + d + y + e;

[@genFlow]
let testCallNamedArgs = (foo: ((~a: int, ~b: int) => int), a, b) =>
  foo(~a, ~b);

[@genFlow]
let testDefaultArgs = (~x=3, ~y) => x + y;