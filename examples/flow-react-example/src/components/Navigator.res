let component = ReasonReact.statelessComponent("Navigator")

type action =
  | Navigate(string)
  | Logout

@genType
let make = (~history, ~match as match_, _children) => {
  ...component,
  render: _self => {
    Js.log2("history", history)
    Js.log2("match", match_)
    <div />
  },
}

