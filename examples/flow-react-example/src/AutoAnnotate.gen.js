/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as AutoAnnotateBS from './AutoAnnotate.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

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

export type r5 = {|+r5: number|};

export type r6 = {|+r6: number|};

export const useR5: (r5) => r5 = function _(Arg1) { const result = AutoAnnotateBS.useR5([Arg1.r5]); return {r5:result[0]} };

export type Props = {|+r6: r6, +children?: mixed|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  AutoAnnotateBS.component,
  (function _(jsProps: Props) {
     return Curry._2(AutoAnnotateBS.make, [jsProps.r6.r6], jsProps.children);
  }));

export default component;
