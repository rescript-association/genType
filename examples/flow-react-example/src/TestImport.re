[@genType.import "./exportNestedValues"]
[@genType.as
  "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents"
]
[@bs.module "./TestImport.gen"]
external innerStuffContents: {. "x": int} = "";

[@genType.import "./exportNestedValues"]
[@genType.as
  "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents"
]
[@bs.module "./TestImport.gen"]
external innerStuffContentsAsEmptyObject: Js.t({.}) = "";

let innerStuffContents = innerStuffContents;

[@genType.import "./exportNestedValues"]
[@genType.as "ValueStartingWithUpperCaseLetter"]
[@bs.module "./TestImport.gen"]
external valueStartingWithUpperCaseLetter: string = "";

[@genType.import "./exportNestedValues"]
[@genType.as "default"]
[@bs.module "./TestImport.gen"]
external defaultValue: int = "";

[@genType]
type message = {text: string};

[@genType.import "./interop/MyBanner.component"]
[@genType.as "TopLevelClass.MiddleLevelElements.MyBannerInternal"]
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