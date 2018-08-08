/* @flow strict */

const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");

export opaque type TypeWithVarsA<y,x> = any // Reason type already checked. Making it opaque;
export const A: <y,x>(x, y) => TypeWithVarsA<y,x> = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type TypeWithVarsB<z> = any // Reason type already checked. Making it opaque;
export const B: <z>(z) => TypeWithVarsB<z> = function _(Arg1) { return CreateBucklescriptBlock.__(1, [Arg1]) }
export type typeWithVars<x,y,z> =
  | TypeWithVarsA<y,x>
  | TypeWithVarsB<z>;

