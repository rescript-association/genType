[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
[@bs.module "./TestImport.gen"]
external innerStuffContents: {. "x": int} = "";

[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
[@bs.module "./TestImport.gen"]
external innerStuffContentsAsEmptyObject: Js.t({.}) = "";

let innerStuffContents = innerStuffContents;

[@genType.import ("./exportNestedValues", "ValueStartingWithUpperCaseLetter")]
[@bs.module "./TestImport.gen"]
external valueStartingWithUpperCaseLetter: string = "";

[@genType.import ("./exportNestedValues", "default")]
[@bs.module "./TestImport.gen"]
external defaultValue: int = "";

[@genType]
type message = {text: string};

[@genType.import
  (
    "./interop/MyBanner.component",
    "TopLevelClass.MiddleLevelElements.MyBannerInternal",
  )
]
[@bs.module "./ImportMyBanner.gen"]
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