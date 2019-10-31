[@genType]
type promise('a) = Js.Promise.t('a);

[@genType]
type fromPayload = {
  [@dead "fromPayload.x"] x: int,
  [@dead "fromPayload.s"] s: string,
};

[@genType]
type toPayload = {[@dead "toPayload.result"] result: string};

[@genType]
let convert = Js.Promise.then_(({s}) => Js.Promise.resolve({result: s}));