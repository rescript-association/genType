[@genType.import ("./exportValues", "default")]
external exportValues: int = "exportValues";

[@genType.import
  (
    "./exportNestedValues",
    "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents",
  )
]
external innerStuffContents: {. "x": int} = "innerStuffContents";

[@genType.import "./exportValues"]
type someType;
