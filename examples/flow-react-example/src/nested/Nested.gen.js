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
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as NestedBS from './Nested.bs';

// flowlint-next-line nonstrict-import:off
import type {variant as Component2_variant} from '../../src/Component2.gen';

export type variant = 
    "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export const consumeVariant: (Component2_variant) => number = function (Arg1: $any) {
  const result = NestedBS.consumeVariant(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? CreateBucklescriptBlock.__(0, Arg1.value)
      : CreateBucklescriptBlock.__(1, [(Arg1.value == null ? undefined : Arg1.value)])
    : $$toRE552311971[Arg1]);
  return result
};
