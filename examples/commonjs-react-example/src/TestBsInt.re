[@genType.import "./bsString"] [@module "TestBsInt.re"]
external packInt:
  ([@bs.int] [ | `a | [@bs.as 34] `removeRange | `normal], string, string) =>
  string =
  "packInt";

let x = packInt(`normal, "a", "b");
