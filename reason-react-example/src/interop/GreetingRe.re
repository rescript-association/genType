/* ReasonReact used by ReactJS */
/* This is just a normal stateless component. The only change you need to turn
   it into a ReactJS-compatible component is the wrapReasonForJs call below */
let component = ReasonReact.statelessComponent("PageReason");

[@genFlow]
let make = (~message, ~extraGreeting=?, _children) => {
  ...component,
  render: _self => {
    let greeting =
      switch (extraGreeting) {
      | None => "How are you?"
      | Some(g) => g
      };
    <div> <MyBannerRe show=true message=(message ++ " " ++ greeting) /> </div>;
  },
};

[@genFlow]
let empty : list(string) = [];

[@genFlow]
let cons = (x,l) => [x, ...l];

[@genFlow]
let concat = String.concat;