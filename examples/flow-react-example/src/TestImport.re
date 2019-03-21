[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
external innerStuffContents: {. "x": int} = "";

[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
external innerStuffContentsAsEmptyObject: Js.t({.}) = "";

let innerStuffContents = innerStuffContents;

[@genType.import ("./exportNestedValues", "ValueStartingWithUpperCaseLetter")]
external valueStartingWithUpperCaseLetter: string = "";

[@genType.import ("./exportNestedValues", "default")]
external defaultValue: int = "";

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
  "";

let make = make;