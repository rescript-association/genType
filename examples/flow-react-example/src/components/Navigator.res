type action =
  | Navigate(string)
  | Logout

@genType @react.component
let make = (~history, ~match as match_) => {
  Js.log2("history", history)
  Js.log2("match", match_)
  <div />
}
