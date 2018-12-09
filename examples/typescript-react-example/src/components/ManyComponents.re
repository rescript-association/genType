module InnerComponent = {
  let component = ReasonReact.statelessComponent("InnerComponent");

  [@genType]
  let make = _children => {
    ...component,
    render: _ => <div> "Inner Component"->ReasonReact.string </div>,
  };
};

let component = ReasonReact.statelessComponent("ManyComponents");

[@genType]
let make = _children => {
  ...component,
  render: _ =>
    <div> {ReasonReact.string("Outer Component")} <InnerComponent /> </div>,
};