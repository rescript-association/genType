[@genType] [@bs.module]
external myBanner: ReasonReact.reactClass = "./MyBanner";

[@bs.deriving abstract]
type jsProps = {
  show: bool,
  message: Js.Nullable.t(string),
};

[@genType]
let make:
  (~show: bool, ~message: Js.Nullable.t(string), 'a) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  );