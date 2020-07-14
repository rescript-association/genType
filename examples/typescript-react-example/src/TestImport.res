@genType.import(
  ("./exportNestedValues", "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents")
)
external innerStuffContents: {"x": int} = "innerStuffContents"

@genType.import(
  ("./exportNestedValues", "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents")
)
external innerStuffContentsAsEmptyObject: {.} = "innerStuffContentsAsEmptyObject"

let innerStuffContents = innerStuffContents

@genType.import(("./exportNestedValues", "ValueStartingWithUpperCaseLetter"))
external valueStartingWithUpperCaseLetter: string = "valueStartingWithUpperCaseLetter"

@genType.import(("./exportNestedValues", "default")) external defaultValue: int = "defaultValue"

export type message = {text: string}

@genType.import(("./MyBanner", "TopLevelClass.MiddleLevelElements.MyBannerInternal"))
external make: (
  ~show: bool,
  ~message: option<message>=?,
  'a,
) => ReasonReact.component<
  ReasonReact.stateless,
  ReasonReact.noRetainedProps,
  ReasonReact.actionless,
> = "make"

let make = make

@genType.import(("./exportNestedValues", "default")) external defaultValue2: int = "defaultValue2"
