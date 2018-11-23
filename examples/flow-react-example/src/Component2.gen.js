/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

export opaque type VariantA = mixed;

export const A: VariantA = 0;

export opaque type VariantB = mixed;

export const B: (number, number) => VariantB = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [Arg1, Arg2]) }

export opaque type VariantC = mixed;

export const C: (?number) => VariantC = function _(Arg1) { return CreateBucklescriptBlock.__(1, [(Arg1 == null ? undefined : Arg1)]) }

export type variant =
  | VariantA
  | VariantB
  | VariantC;

export opaque type BlockBlock = mixed;

export const Block: BlockBlock = 0;

export type block =
  | BlockBlock;

export const getBlock: (block) => number = Component2BS.getBlock;
