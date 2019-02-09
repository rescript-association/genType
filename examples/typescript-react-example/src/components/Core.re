let component = ReasonReact.statelessComponent("Core");

/** This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason */
[@genType]
let make =
    (
      ~imageComponent,
      ~title: string,
      ~description: string,
      ~actionButton=?,
      _children,
    ) => {
  ...component,
  render: _self =>
    <div className="flex flex-col items-center justify-center">
      imageComponent
      <div>
        <div> {ReasonReact.string(title)} </div>
        <div> {ReasonReact.string(description)} </div>
        {switch (actionButton) {
         | Some(button) => button
         | None => ReasonReact.null
         }}
      </div>
    </div>,
};