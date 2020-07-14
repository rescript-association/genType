module Inner = {
  export x = 34
  export y = "hello"
}

module Outer = {
  module Medium = {
    module Inner = {
      export y = 44
    }
  }
}
