/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Component1BS from './Component1.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// flowlint-next-line nonstrict-import:off
import type {list} from '../src/shims/ReasonPervasives.shim';

// flowlint-next-line nonstrict-import:off
import type {variant as Component2_variant} from './Component2.gen';

export type Props = {| +message?: string, +children?: mixed |};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  Component1BS.component,
  (function _(jsProps: Props) {
     return Curry._2(Component1BS.make, jsProps.message, jsProps.children);
  }));

export default component;

export const plus: <T1>(number, T1) => number = Component1BS.plus;

export const concat: (string, ?string) => ?string = function _(Arg1, Arg2) {
  const result = Component1BS.concat(Arg1, (Arg2 == null ? undefined : Arg2));
  return result
};

export const consumeVariant: (Component2_variant) => number = Component1BS.consumeVariant;

export const l: list<number> = Component1BS.l;

export const map: <T1,T2>(((T1) => T2), list<T1>) => list<T2> = Component1BS.map;
