[@genType.import "./exportNestedValues"]
[@genType.as
  "TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents"
]
[@bs.module "./TestImport.gen"]
external innerStuffContents: {. "x": int} = "";

let innerStuffContents = innerStuffContents;

[@genType.import "./exportNestedValues"]
[@genType.as "ValueStartingWithUpperCaseLetter"]
[@bs.module "./TestImport.gen"]
external valueStartingWithUpperCaseLetter: string = "";