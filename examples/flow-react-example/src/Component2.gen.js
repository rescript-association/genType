/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

export type variant = 
  | "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export type block = "Block";

export const getBlock: (block) => number = function (Arg1: $any) {
  const result = Component2BS.getBlock(0);
  return result
};
