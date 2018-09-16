/* ReasonReact used by ReactJS */
/* This is just a normal stateless component. The only change you need to turn
   it into a ReactJS-compatible component is the wrapReasonForJs call below */
let component = ReasonReact.statelessComponent("PageReason");

[@genType]
let make = (~message, ~someNumber, ~extraGreeting=?, ~polymorphicProp, _children) => {
  ...component,
  render: _self => {
    Js.log2("polymorphicProp:", polymorphicProp);
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

[@genType]
let empty: list(string) = [];

[@genType]
let cons = (~x, ~l) => [x, ...l];

[@genType]
let cons2 = (~l, ~x) => [x, ...l];

[@genType]
let concat = String.concat;

[@genType]
let testNamedArgs = (~a, ~b, x, ~c, ~d, y, ~e) => a + b + x + c + d + y + e;

[@genType]
let testCallNamedArgs = (foo: ((~a: int, ~b: int) => int), a, b) =>
  foo(~a, ~b);

[@genType]
let testDefaultArgs = (~x=3, ~y) => x + y;