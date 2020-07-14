export type promise<'a> = Js.Promise.t<'a>

export type fromPayload = {
  x: int,
  s: string,
}

export type toPayload = {result: string}

export convert = Js.Promise.then_(({s}) => Js.Promise.resolve({result: s}))
