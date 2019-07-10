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
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContentsEmpty",
  )
]
external innerStuffContentsAsEmptyObject: Js.t({.}) =
  "innerStuffContentsAsEmptyObject";

let innerStuffContents = innerStuffContents;

[@genType.import ("./exportNestedValues", "ValueStartingWithUpperCaseLetter")]
external valueStartingWithUpperCaseLetter: string =
  "valueStartingWithUpperCaseLetter";

[@genType.import ("./exportNestedValues", "default")]
external defaultValue: int = "defaultValue";

[@genType]
type message = {text: string};

[@genType.import
  (
    "./interop/MyBanner.component",
    "TopLevelClass.MiddleLevelElements.MyBannerInternal",
  )
]
external make:
  (~show: bool) =>
  [@genType.as "Message"] (
    (~message: Js.Nullable.t(string), 'a) =>
    ReasonReact.component(
      ReasonReact.stateless,
      ReasonReact.noRetainedProps,
      ReasonReact.actionless,
    )
  ) =
  "make";

let make = make;