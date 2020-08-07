/** 
 * @flow strict
 * @generated from Nested.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

const $$toRE552311971 = {"A": 0};

// $FlowExpectedError: Reason checked type sufficiently
import * as NestedBS from './Nested.bs';

import type {variant as Component2_variant} from '../../src/Component2.gen';

export type variant = 
    "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export const consumeVariant: (Component2_variant) => number = function (Arg1: $any) {
  const result = NestedBS.consumeVariant(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value[0], _1:Arg1.value[1]}
      : {TAG: 1, _0:(Arg1.value == null ? undefined : Arg1.value)}
    : $$toRE552311971[Arg1]);
  return result
};
