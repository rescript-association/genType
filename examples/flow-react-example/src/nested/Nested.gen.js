/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as NestedBS from './Nested.bs';

// flowlint-next-line nonstrict-import:off
import type {variant as Component2_variant} from '../../src/Component2.gen';

export opaque type VariantA = mixed;

export const A: VariantA = 0;

export opaque type VariantB = mixed;

export const B: (number, number) => VariantB = function _(VArg1, VArg2) { return CreateBucklescriptBlock.__(0, [VArg1, VArg2]) }

export opaque type VariantC = mixed;

export const C: (?number) => VariantC = function _(VArg1) { return CreateBucklescriptBlock.__(1, [(VArg1 == null ? undefined : VArg1)]) }

export type variant =
  | VariantA
  | VariantB
  | VariantC;

export const consumeVariant: (Component2_variant) => number = NestedBS.consumeVariant;
