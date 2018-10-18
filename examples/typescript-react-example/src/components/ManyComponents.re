module InnerComponent = {
  let component = ReasonReact.statelessComponent("InnerComponent");

  [@genType]
  let make = _children => {
    ...component,
    render: _ => ReasonReact.string("Inner Component"),
  };
};

let component = ReasonReact.statelessComponent("ManyComponents");

let make = _children => {
  ...component,
  render: _ =>
    <div> {ReasonReact.string("Outer Component")} <InnerComponent /> </div>,
};