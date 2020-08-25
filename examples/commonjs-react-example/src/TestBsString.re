[@genType.import "./bsString"]
[@module "TestBsString.re"]
external pack:
  (
    [@bs.string] [ | [@bs.as "remove-range"] `removeRange | `normal],
    string,
    string
  ) =>
  string =
  "pack";

let x = pack(`removeRange, "a", "b");
