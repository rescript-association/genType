/* @flow strict */

const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");

export opaque type TypeWithVarsA<x,y> = any;
export const A: <x,y>(x, y) => TypeWithVarsA<x,y> = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type TypeWithVarsB<z> = any;
export const B: <z>(z) => TypeWithVarsB<z> = function _(Arg1) { return CreateBucklescriptBlock.__(1, [Arg1]) }
export type typeWithVars<x,y,z> =
  | TypeWithVarsA<x,y>
  | TypeWithVarsB<z>;
export opaque type coord = any; /* Record type not supported */

