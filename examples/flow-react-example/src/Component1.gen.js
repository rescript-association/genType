/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

const $$toRE552311971 = {"A": 0};

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as Component1BS from './Component1.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

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

export const plus: <T1>(number, T1) => number = function _<T1>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.plus, Arg1, Arg2);
  return result
};

export const concat: (string, ?string) => ?string = function _(Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.concat, Arg1, (Arg2 == null ? undefined : Arg2));
  return result
};

export const consumeVariant: (Component2_variant) => number = function _(Arg1: $any) {
  const result = Component1BS.consumeVariant(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? CreateBucklescriptBlock.__(0, Arg1.value)
      : CreateBucklescriptBlock.__(1, [(Arg1.value == null ? undefined : Arg1.value)])
    : $$toRE552311971[Arg1]);
  return result
};

export const l: list<number> = Component1BS.l;

export const map: <T1,T2>(((T1) => T2), list<T1>) => list<T2> = function _<T1,T2>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.map, Arg1, Arg2);
  return result
};
