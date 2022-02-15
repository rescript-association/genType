/** 
 * @flow strict
 * @generated from Component2.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

export type variant = 
    "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export type block = "Block";

export type Props = {| +greeting: string |};

export const make: React$ComponentType<{| +greeting: string |}> = Component2BS.make;

export const getBlock: (block) => number = function (Arg1: $any) {
  const result = Component2BS.getBlock(0);
  return result
};
