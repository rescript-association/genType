export notNested = 1

module Universe = {
  export theAnswer = 42

  let notExported = 33

  export type nestedType = array<string>

  module Nested2 = {
    let x = 0

    export nested2Value = 1

    let y = 2

    export type nested2Type = array<array<string>>

    module Nested3 = {
      let x = 0
      let y = 1
      let z = 2
      let w = 3

      export type nested3Type = array<array<array<string>>>

      export nested3Value = "nested3Value"

      export nested3Function = (x: nested2Type) => x
    }

    export nested2Function = (x: Nested3.nested3Type) => x
  }

  export type variant =
    | A
    | B(string)

  export someString = "some exported string"
}
