/* The @genFlow annotation is ignored if there's also a .rei file */
[@genFlow] [@bs.module]
external myBanner: ReasonReact.reactClass = "./MyBanner";

[@bs.deriving abstract]
type jsProps = {
  show: bool,
  message: string,
};

/* The @genFlow annotation is ignored if there's also a .rei file */
[@genFlow]
let make = (~show, ~message, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=myBanner,
    ~props=jsProps(~show, ~message),
    children,
  );