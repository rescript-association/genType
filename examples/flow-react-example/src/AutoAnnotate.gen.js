/** 
 * @flow strict
 * @generated from AutoAnnotate.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as AutoAnnotateBS from './AutoAnnotate.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type variant = {| tag: "R", value: number |};

export type record = {| +variant: variant |};

export type r2 = {| +r2: number |};

export type r3 = {| +r3: number |};

export type r4 = {| +r4: number |};

export type annotatedVariant = 
    {| tag: "R2", value: [r2, r3] |}
  | {| tag: "R4", value: r4 |};

export type r5 = {| +r5: number |};

export type r6 = {| +r6: number |};

export const useR5: (r5) => r5 = AutoAnnotateBS.useR5;

export type Props = {| +r6: r6, +children?: mixed |};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  AutoAnnotateBS.component,
  (function _(jsProps: Props) {
     return Curry._2(AutoAnnotateBS.make, jsProps.r6, jsProps.children);
  }));

export default component;
