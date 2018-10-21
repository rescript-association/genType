[@genType]
let useGetProp = (x: WrapJsValue.AbsoluteValue.t) =>
  x->WrapJsValue.AbsoluteValue.getProp + 1;

[@genType]
let useTypeImportedInOtherModule = (x: WrapJsValue.stringFunction) => x;