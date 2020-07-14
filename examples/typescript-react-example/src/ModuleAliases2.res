export type record = {
  x: int,
  y: string,
}

module Outer = {
  export type outer = {outer: string}

  module Inner = {
    export type inner = {inner: string}
  }
}

module OuterAlias = Outer

module InnerAlias = OuterAlias.Inner

let q = 42
