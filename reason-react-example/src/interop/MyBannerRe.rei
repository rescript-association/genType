[@genFlow] [@bs.module]
external myBanner: ReasonReact.reactClass = "./MyBanner";

[@bs.deriving abstract]
type jsProps = {
  show: bool,
  message: string,
};

[@genFlow]
let make:
  (~show: bool, ~message: string, 'a) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  );