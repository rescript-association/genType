/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as AutoAnnotateBS from './AutoAnnotate.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type variant = "VariantR";

export type record = {|+variant: variant|};

export type annotatedVariant = "AnnotatedVariantR2" | "AnnotatedVariantR4";

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
