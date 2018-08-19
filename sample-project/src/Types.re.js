/* @flow strict */

const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
const TypesBS = require("./Types.bs");

// No need to import locally visible type optionInt. Make sure it is also marked with @genFlow

export opaque type TypeWithVarsA<x,y> = any;
export const A: <x,y>(x, y) => TypeWithVarsA<x,y> = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type TypeWithVarsB<z> = any;
export const B: <z>(z) => TypeWithVarsB<z> = function _(Arg1) { return CreateBucklescriptBlock.__(1, [Arg1]) }
export type typeWithVars<x,y,z> =
  | TypeWithVarsA<x,y>
  | TypeWithVarsB<z>;
export type coord = {|x:number, y:number, z?:number|};
export type optionInt = ?number;
export const consumeOption: (?number) => number = function _(Arg1) { const result = TypesBS.consumeOption((Arg1 === null ? undefined : Arg1)); return result };
export const consumeOption2: (optionInt) => number = function _(Arg1) { const result = TypesBS.consumeOption2((Arg1 === null ? undefined : Arg1)); return result };