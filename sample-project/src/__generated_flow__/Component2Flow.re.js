/* @flow strict */

var CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");

export opaque type VariantA = any // Reason type already checked. Making it opaque;
const A = 0;
export opaque type VariantB = any // Reason type already checked. Making it opaque;
function B(Arg0, Arg1) { return CreateBucklescriptBlock.__(0, [Arg0, Arg1]) }
export opaque type VariantC = any // Reason type already checked. Making it opaque;
function C(Arg0) { return CreateBucklescriptBlock.__(1, [(Arg0 === null ? undefined : Arg0)]) }
export type Variant =
  | VariantA
  | VariantB
  | VariantC;

exports.A = (A: VariantA);
exports.B = (B: (number, number) => VariantB);
exports.C = (C: (?number) => VariantC);