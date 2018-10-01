/* @flow strict */

const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
const NestedBS = require("./Nested.bs");

import type {variant as Component2_variant} from '../../src/basics/Component2.re';

export opaque type VariantA = any;
export const A: VariantA = 0;
export opaque type VariantB = any;
export const B: (number, number) => VariantB = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
export opaque type VariantC = any;
export const C: (?number) => VariantC = function _(Arg1) { return CreateBucklescriptBlock.__(1, [(Arg1 == null ? undefined : Arg1)]) }
export type variant =
  | VariantA
  | VariantB
  | VariantC;
export const consumeVariant: (Component2_variant) => number = NestedBS.consumeVariant;