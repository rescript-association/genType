let component = ReasonReact.statelessComponent("Navigator");

type action =
  | Navigate(string)
  | Logout;

[@genType]
let make = (~history, ~match, _children) => {
  ...component,
  render: _self => {
    Js.log2("history", history);
    Js.log2("match", match);
    <div />;
  },
};