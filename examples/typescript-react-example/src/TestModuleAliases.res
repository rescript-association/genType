module OtherFile = ModuleAliases2
module OtherFileAlias = OtherFile

export type record = OtherFile.record

export type record2 = OtherFileAlias.record

module OuterAlias = OtherFile.Outer

export type outer = OtherFileAlias.Outer.outer

export type outer2 = OuterAlias.outer

module OtherFile1 = OtherFile
module Outer2 = OtherFile1.Outer
module Inner2 = Outer2.Inner

export type my2 = Inner2.inner

export type inner1 = OtherFile.InnerAlias.inner

export type inner2 = OtherFile.Outer.Inner.inner

export testInner1 = (x: inner1) => x

export testInner1Expanded = (x: OtherFile.InnerAlias.inner) => x

export testInner2 = (x: inner2) => x

export testInner2Expanded = (x: OtherFile.Outer.Inner.inner) => x
