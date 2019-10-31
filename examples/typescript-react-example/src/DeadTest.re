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