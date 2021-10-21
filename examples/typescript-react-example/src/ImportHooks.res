@genType
type person = {
  name: string,
  age: int,
}

@genType type renderMe<'a> = React.component<{"randomString": string, "poly": 'a}>

@genType.import("./hookExample") @react.component
external make: (
  ~actions: React.element=?,
  ~person: person,
  ~children: React.element,
  ~renderMe: renderMe<'a>,
) => React.element = "makeRenamed"

@genType.import("./hookExample") external foo: (~person: person) => string = "foo"
