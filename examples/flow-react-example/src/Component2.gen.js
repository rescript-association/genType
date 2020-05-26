/** 
 * @flow strict
 * @generated from Component2.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

export type variant = 
    "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export type block = "Block";

// Type annotated function components are not checked by Flow, but typeof() works.
const make$$forTypeof = function (_: {| +greeting: string |}) : React$Node { return null };

export type Props = {| +greeting: string |};

export const make: typeof(make$$forTypeof) = Component2BS.make;

export const getBlock: (block) => number = function (Arg1: $any) {
  const result = Component2BS.getBlock(0);
  return result
};
