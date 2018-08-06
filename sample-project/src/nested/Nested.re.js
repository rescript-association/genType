/* @flow strict */

const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
const NestedBS = require("./Nested.bs");

import type {variant as Component2variant} from '../../src/Component2.re';
export opaque type VariantA = any // Reason type already checked. Making it opaque;
export const A: VariantA = 0;
export opaque type VariantB = any // Reason type already checked. Making it opaque;
export const B: (number, number) => VariantB = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type VariantC = any // Reason type already checked. Making it opaque;
export const C: (?number) => VariantC = function _(Arg1) { return CreateBucklescriptBlock.__(1, [(Arg1 === null ? undefined : Arg1)]) }
export type variant =
  | VariantA
  | VariantB
  | VariantC;
export const consumeVariant: (Component2variant) => number = NestedBS.consumeVariant;

