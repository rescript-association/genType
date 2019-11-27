/** 
 * @flow strict
 * @generated from Component2.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type variant = 
    "A"
  | {| tag: "B", value: [number, number] |}
  | {| tag: "C", value: ?number |};

export type block = "Block";

export type Props = {| +greeting: string, +children?: mixed |};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  Component2BS.component,
  (function _(jsProps: Props) {
     return Curry._2(Component2BS.make, jsProps.greeting, jsProps.children);
  }));

export default component;

export const getBlock: (block) => number = function (Arg1: $any) {
  const result = Component2BS.getBlock(0);
  return result
};
