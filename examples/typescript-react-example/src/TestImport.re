[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
external innerStuffContents: {. "x": int} = "innerStuffContents";

[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
external innerStuffContentsAsEmptyObject: Js.t({.}) =
  "innerStuffContentsAsEmptyObject";

[@dead "innerStuffContents"] let innerStuffContents = innerStuffContents;

[@genType.import ("./exportNestedValues", "ValueStartingWithUpperCaseLetter")]
external valueStartingWithUpperCaseLetter: string =
  "valueStartingWithUpperCaseLetter";

[@genType.import ("./exportNestedValues", "default")]
external defaultValue: int = "defaultValue";

[@genType]
type message = {[@dead "message.text"] text: string};

[@genType.import
  ("./MyBanner", "TopLevelClass.MiddleLevelElements.MyBannerInternal")
]
external make:
  (~show: bool, ~message: option(message)=?, 'a) =>
  ReasonReact.component(
    ReasonReact.stateless,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  ) =
  "make";

[@dead "make"] let make = make;

[@genType.import ("./exportNestedValues", "default")]
external defaultValue2: int = "defaultValue2";