/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as Component2BS from './Component2.bs';

export type variant = "A" | {|tag: "B", value: [number, number]|} | {|tag: "C", value: [?number]|};

export type block = "Block";

export const getBlock: (block) => number = function _(Arg1) { const result = Component2BS.getBlock(0); return result };
