open Belt

export type coord = {
  x: int,
  y: int,
  z: option<int>,
}

export origin = {x: 0, y: 0, z: Some(0)}

export computeArea = ({x, y, z}) => {
  open Option
  x * y * z->mapWithDefault(1, n => n)
}

export coord2d = (x, y) => {x: x, y: y, z: None}

export type person = {
  name: string,
  age: int,
  address: option<string>,
}

export type business = {
  name: string,
  owner: option<person>,
  address: option<string>,
}

let getOpt = (opt, default, foo) => opt->Option.mapWithDefault(default, foo)

export findAddress = (business: business): list<string> =>
  business.address->getOpt(list{}, a => list{a})

export someBusiness = {name: "SomeBusiness", owner: None, address: None}

export findAllAddresses = (businesses: array<business>): array<string> =>
  businesses
  ->Array.map(business =>
    \"@"(
      business.address->getOpt(list{}, a => list{a}),
      business.owner->getOpt(list{}, p => p.address->getOpt(list{}, a => list{a})),
    )
  )
  ->List.fromArray
  ->List.flatten
  ->List.toArray

export type payload<'a> = {
  num: int,
  payload: 'a,
}

export getPayload = ({payload}) => payload

export type record = {
  v: int,
  w: int,
}

export getPayloadRecord = ({payload}): record => payload

export recordValue = {v: 1, w: 1}

export payloadValue = {num: 1, payload: recordValue}

export getPayloadRecordPlusOne = ({payload}): record => {
  ...payload,
  v: payload.v + 1,
}

export type business2 = {
  name: string,
  owner: Js.Nullable.t<person>,
  address2: Js.Nullable.t<string>,
}

export findAddress2 = (business: business2): list<string> =>
  business.address2->Js.Nullable.toOption->getOpt(list{}, a => list{a})

export someBusiness2 = {
  name: "SomeBusiness",
  owner: Js.Nullable.null,
  address2: Js.Nullable.null,
}

export computeArea3 = (o: {"x": int, "y": int, "z": Js.Nullable.t<int>}) =>
  o["x"] * o["y"] * o["z"]->Js.Nullable.toOption->Option.mapWithDefault(1, n => n)

export computeArea4 = (o: {"x": int, "y": int, "z": option<int>}) =>
  o["x"] * o["y"] * o["z"]->Option.mapWithDefault(1, n => n)

export type mix = {"a": int, "b": int, "c": option<{"name": string, "surname": string}>}

export type myRec = {
  @genType.as("type")
  type_: string,
}

export type myObj = {"type_": string}

export testMyRec = (x: myRec) => x.type_

export testMyRec2 = (x: myRec) => x

export testMyObj = (x: myObj) => x["type_"]

export testMyObj2 = (x: myObj) => x

export type myRecBsAs = {
  @bs.as("jsValid0")
  valid: string,
  @as("type")
  type_: string,
  @bs.as("the-key")
  theKey: string,
  @bs.as("with\"dquote")
  withDQuote: string,
  @bs.as("with'squote")
  withSQuote: string,
  @bs.as("1number")
  number1: string,
}

export testMyRecBsAs = (x: myRecBsAs) => [
  x.valid,
  x.type_,
  x.theKey,
  x.withDQuote,
  x.withSQuote,
  x.number1,
]

export testMyRecBsAs2 = (x: myRecBsAs) => x
