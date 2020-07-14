// Test pervasive references

export create = (x: int) => ref(x)

export access = r => r.contents + 1

export update = r => r.contents = r.contents + 1

// Abstract version of references: works when conversion is required.

module R: {
  export type t<'a>
  let get: t<'a> => 'a
  let make: 'a => t<'a>
  let set: (t<'a>, 'a) => unit
} = {
  type t<'a> = ref<'a>
  let get = r => r.contents
  let make = ref
  let set = (r, v) => r.contents = v
}

export type t<'a> = R.t<'a>

export get = R.get

@gentype
let make = R.make

export set = R.set

type requiresConversion = {x: int}

// Careful: conversion makes a copy and destroys the reference identity.
export destroysRefIdentity = (x: ref<requiresConversion>) => x

// Using abstract references preserves the identity.
export preserveRefIdentity = (x: R.t<requiresConversion>) => x
