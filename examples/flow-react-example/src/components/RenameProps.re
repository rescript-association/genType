let component = ReasonReact.statelessComponent("RenameProps");

/* All these genType.as annotation are picked up in the AST. */
[@genType]
type functionTypeWithGenTypeAs =
  [@genType.as "type"] (
    (~type_: string) => [@genType.as "$number"] ((~number: int) => int)
  );

[@genType]
let functionWithGenTypeAs = (~firstNameArgumentCantBeRenamed) =>
  [@genType.as "type"]
  (
    (~type_) =>
      [@genType.as "$$number"]
      (
        (~number) =>
          firstNameArgumentCantBeRenamed ++ type_ ++ string_of_int(number)
      )
  );

/* These genType.as annotation are currently not picked up in the AST */
[@genType]
let make = (~firstNameArgumentCantBeRenamed) =>
  [@genType.as "type"]
  (
    (~type_) =>
      [@genType.as "$$number"]
      (
        (~number, _children) => {
          ...component,
          render: _self =>
            (firstNameArgumentCantBeRenamed ++ type_ ++ string_of_int(number))
            ->ReasonReact.string,
        }
      )
  );

[@genType]
let firstIsIgnored = [@genType.as "Ignored"] ((~x) => x + 0);

[@genType]
let padding1 = pad => [@genType.as "xRenamed"] ((~x) => pad + x);

[@genType]
let padding2 = (~pad) => [@genType.as "xRenamed"] ((~x) => pad + x);

[@genType]
let padding3 = (pad1, pad2) =>
  [@genType.as "xRenamed"] ((~x) => pad1 + pad2 + x);

[@genType]
let renameABunch = pad =>
  [@genType.as "xRenamed"]
  (
    (~x) =>
      [@genType.as "yRenamed"]
      ((~y) => [@genType.as "zRenamed"] ((~z) => pad + x + y + z))
  );

[@genType]
let renameABunch2 = pad =>
  [@genType.as "xRenamed"]
  ((~x, ~y) => [@genType.as "zRenamed"] ((~z) => pad + x + y + z));

[@genType]
let renameABunch3 = pad =>
  [@genType.as "xRenamed"]
  ((~x, y) => [@genType.as "zRenamed"] ((~z) => pad + x + y + z));