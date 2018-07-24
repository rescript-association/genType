/* @flow strict */

var CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");

export opaque type VariantA = any // Reason type already checked. Making it opaque;
const A = 0;
export opaque type VariantB = any // Reason type already checked. Making it opaque;
function B(Arg1, Arg2) { CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type VariantC = any // Reason type already checked. Making it opaque;
function C(Arg1) { CreateBucklescriptBlock.__(1, [(Arg1 === null ? undefined : Arg1)]) }
export type Variant =
  | VariantA
  | VariantB
  | VariantC;

exports.A = (A: VariantA);
exports.B = (B: (number, number) => VariantB);
exports.C = (C: (?number) => VariantC);