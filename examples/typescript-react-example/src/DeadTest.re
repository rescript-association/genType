let _ = Js.log(ImmutableArray.fromArray);
let fortytwo = 42;

[@genType]
let fortyTwoButExported = 42;

let thisIsUsedOnce = 34;
ignore(thisIsUsedOnce);

let thisIsUsedTwice = 34;
ignore(thisIsUsedTwice);
ignore(thisIsUsedTwice);

[@dead]
let thisIsMarkedDead = 99;

let thisIsKeptAlive = 42;

[@live]
let thisIsMarkedLive = thisIsKeptAlive;

module Inner = {
  [@dead]
  let thisIsAlsoMarkedDead = 99;
};

module M: {
  [@dead]
  let thisSignatureItemIsDead: int;
} = {
  let thisSignatureItemIsDead = 34;
};

module VariantUsedOnlyInImplementation: {
  type t =
    | A; // TODO: discovered this automatically
  let a: t;
} = {
  type t =
    | A;
  let a = A;
};

let _ = VariantUsedOnlyInImplementation.a;

let _ = DeadTypeTest.OnlyInInterface;
let _ = DeadTypeTest.InBoth;