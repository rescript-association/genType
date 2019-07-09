[@genType.import ("./hookExample", "default")] [@react.component]
external make: (~show: bool, ~_Message: string=?) => React.element = "make";

[@genType.import "./hookExample"] [@react.component]
external make2: (~show: bool, ~_Message: string=?) => React.element =
  "default";