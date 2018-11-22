/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/js/block.js';

export opaque type VariantR = mixed;

export const R: (number) => VariantR = function _(Arg1) { return CreateBucklescriptBlock.__(0, [Arg1]) }

export type variant =
  | VariantR;

export type record = {|+variant: variant|};

export type r2 = {|+r2: number|};

export type r3 = {|+r3: number|};

export type r4 = {|+r4: number|};

export opaque type AnnotatedVariantR2 = mixed;

export const R2: (r2, r3) => AnnotatedVariantR2 = function _(Arg1, Arg2) { return CreateBucklescriptBlock.__(0, [[Arg1.r2], [Arg2.r3]]) }

export opaque type AnnotatedVariantR4 = mixed;

export const R4: (r4) => AnnotatedVariantR4 = function _(Arg1) { return CreateBucklescriptBlock.__(1, [[Arg1.r4]]) }

export type annotatedVariant =
  | AnnotatedVariantR2
  | AnnotatedVariantR4;
