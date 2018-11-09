/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/js/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as NestedBS from './Nested.bs';

// flowlint-next-line nonstrict-import:off
import type {variant as Component2_variant} from '../../src/basics/Component2.gen';

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

export const consumeVariant: (Component2_variant) => number = NestedBS.consumeVariant;
