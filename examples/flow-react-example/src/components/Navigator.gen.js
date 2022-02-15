/** 
 * @flow strict
 * @generated from Navigator.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as NavigatorBS from './Navigator.bs';

// flowlint-next-line nonstrict-import:off
import type {actionless as ReasonReact_actionless} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {componentSpec as ReasonReact_componentSpec} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {noRetainedProps as ReasonReact_noRetainedProps} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {stateless as ReasonReact_stateless} from '../../src/shims/ReactShim.shim';

export const make: <T1,T2,T3>({| +history: T1, +match: T2 |}, T3) => ReasonReact_componentSpec<ReasonReact_stateless,ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_noRetainedProps,ReasonReact_actionless> = function <T1,T2,T3>(Arg1: $any, Arg2: $any) {
  const result = Curry._3(NavigatorBS.make, Arg1.history, Arg1.match, Arg2);
  return result
};
