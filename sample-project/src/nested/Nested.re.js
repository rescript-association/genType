/* @flow strict */

var CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
var Nested = require("./Nested.bs");

import type {variant as Component2variant} from '../../src/Component2.re';
export opaque type VariantA = any // Reason type already checked. Making it opaque;
const A = 0;
export opaque type VariantB = any // Reason type already checked. Making it opaque;
function B(Arg1, Arg2) { CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type VariantC = any // Reason type already checked. Making it opaque;
function C(Arg1) { CreateBucklescriptBlock.__(1, [(Arg1 === null ? undefined : Arg1)]) }
export type variant =
  | VariantA
  | VariantB
  | VariantC;
const consumeVariant = Nested.consumeVariant;

exports.A = (A: VariantA);
exports.B = (B: (number, number) => VariantB);
exports.C = (C: (?number) => VariantC);
exports.consumeVariant = (consumeVariant: (Component2variant) => number);