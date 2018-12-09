module InnerComponent = {
  let component = ReasonReact.statelessComponent("InnerComponent");

  [@genType]
  let make = _children => {
    ...component,
    render: _ => <div> "Inner Component"->ReasonReact.string </div>,
  };
};

module ManyProps = {
  let component = ReasonReact.statelessComponent("ManyProps");

  [@genType]
  let make =
      (
        ~a as _,
        ~b as _,
        ~c as _,
        ~d as _,
        ~e as _,
        ~f as _,
        ~g as _,
        ~h as _,
        _children,
      ) => {
    ...component,
    render: _ => ReasonReact.string("Inner Component"),
  };
};

let component = ReasonReact.statelessComponent("ManyComponents");

[@genType]
let make = _children => {
  ...component,
  render: _ =>
    <div> {ReasonReact.string("Outer Component")} <InnerComponent /> </div>,
};