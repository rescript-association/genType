/** 
 * @flow strict
 * @generated from Component1.re
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

// flowlint-next-line nonstrict-import:off
import type {list} from '../src/shims/ReasonPervasives.shim';

import type {variant as Component2_variant} from './Component2.gen';

// Type annotated function components are not checked by Flow, but typeof() works.
const make$$forTypeof = function (_: {| +message?: string |}) : React$Node { return null };

export type Props = {| +message?: string |};

export const make: typeof(make$$forTypeof) = Component1BS.make;

export const plus: <T1>(number, T1) => number = function <T1>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.plus, Arg1, Arg2);
  return result
};

export const concat: (string, ?string) => ?string = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.concat, Arg1, (Arg2 == null ? undefined : Arg2));
  return result
};

export const consumeVariant: (Component2_variant) => number = function (Arg1: $any) {
  const result = Component1BS.consumeVariant(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value[0], _1:Arg1.value[1]}
      : {TAG: 1, _0:(Arg1.value == null ? undefined : Arg1.value)}
    : $$toRE552311971[Arg1]);
  return result
};

export const l: list<number> = Component1BS.l;

export const map: <T1,T2>(((T1) => T2), list<T1>) => list<T2> = function <T1,T2>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(Component1BS.map, Arg1, Arg2);
  return result
};
