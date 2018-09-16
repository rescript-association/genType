/* The @genType annotation is ignored if there's also a .rei file */
[@genType] [@bs.module]
external myBanner: ReasonReact.reactClass = "./MyBanner";

[@bs.deriving abstract]
type jsProps = {
  show: bool,
  message: string,
};

/* The @genType annotation is ignored if there's also a .rei file */
[@genType]
let make = (~show, ~message, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=myBanner,
    ~props=jsProps(~show, ~message),
    children,
  );