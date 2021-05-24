/** 
 * @flow strict
 * @generated from AutoAnnotate.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as AutoAnnotateBS from './AutoAnnotate.bs';

// flowlint-next-line nonstrict-import:off
import type {actionless as ReasonReact_actionless} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {componentSpec as ReasonReact_componentSpec} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {noRetainedProps as ReasonReact_noRetainedProps} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {stateless as ReasonReact_stateless} from '../src/shims/ReactShim.shim';

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

export const make: <T1>({| +r6: r6 |}, T1) => ReasonReact_componentSpec<ReasonReact_stateless,ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_noRetainedProps,ReasonReact_actionless> = function <T1>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(AutoAnnotateBS.make, Arg1.r6, Arg2);
  return result
};
