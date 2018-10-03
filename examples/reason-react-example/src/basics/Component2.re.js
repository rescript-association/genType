/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const Component2BS = require("./Component2.bs");
// $FlowExpectedError: Reason checked type sufficiently
const CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");

// No need to import locally visible type block. Make sure it is also marked with @genType

// $FlowExpectedError: Reason checked type sufficiently
export opaque type VariantA = any;
export const A: VariantA = 0;
// $FlowExpectedError: Reason checked type sufficiently
export opaque type VariantB = any;
export const B: (number, number) => VariantB = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }
// $FlowExpectedError: Reason checked type sufficiently
export opaque type VariantC = any;
export const C: (?number) => VariantC = function _(Arg1) { return CreateBucklescriptBlock.__(1, [(Arg1 == null ? undefined : Arg1)]) }
export type variant =
  | VariantA
  | VariantB
  | VariantC;
// $FlowExpectedError: Reason checked type sufficiently
export opaque type BlockBlock = any;
export const Block: BlockBlock = 0;
export type block =
  | BlockBlock;
export const getBlock: (block) => number = Component2BS.getBlock;