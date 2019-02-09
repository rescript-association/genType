let component = ReasonReact.statelessComponent("ComponentAsProp");

/** This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason */
[@genType]
let make = (~title, ~description, ~button=?, _children) => {
  ...component,
  render: _self =>
    <div>
      <div>
        title
        description
        {switch (button) {
         | Some(button) => button
         | None => ReasonReact.null
         }}
      </div>
    </div>,
};