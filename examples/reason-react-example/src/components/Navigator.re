let component = ReasonReact.statelessComponent("Navigator");

type action =
  | Navigate(string)
  | Logout;

[@genType]
let make = (~history, ~match, _children) => {
  ...component,
  render: _self => {
    Js.log2("history", history);
    Js.log2("match", match);
    <div />;
  },
};

module Design1 = {
  /* These genType.as annotation are picked up in the AST. */
  [@genType]
  type functionTypeWithGenTypeAs =
    [@genType.as "type"] (
      (~type_: string) => [@genType.as "$number"] ((~number: int) => int)
    );

  /* These genType.as annotation are currently not picked up in the AST */
  [@genType]
  let make =
    [@genType.as "type"]
    (
      (~type_) =>
        [@genType.as "$number"]
        (
          (~number, _children) => {
            ...component,
            render: _self => {
              Js.log2("type", type_);
              Js.log2("$number", number);
              <div />;
            },
          }
        )
    );
};

/* module Design2 = {
     [@genType.as [("type_", "type"), ("number", "$number")]]
     let make = (~type_, ~number, _children) => {
       ...component,
       render: _self => {
         Js.log2("type", type_);
         Js.log2("$number", number);
         <div />;
       },
     };
   }; */