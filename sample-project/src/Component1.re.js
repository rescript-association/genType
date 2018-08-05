/* @flow strict */

const Component1BS = require("./Component1.bs");
const ReasonReact = require("reason-react/src/ReasonReact.js");

import type {list} from '../src/shims/ReasonPervasives.shim.js';
import type {variant as Component2variant} from './Component2.re';
export type Props = {|message?:string, children?:any|};
export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  Component1BS.component,
  (function _(jsProps: Props) {
     return Component1BS.make(jsProps.message, jsProps.children);
  }));
export const plus: <T11009>(_:number, _:T11009) => number = Component1BS.plus;
export const concat: (_:string, _:?string) => ?string = function _(Arg1, Arg2) { const result = Component1BS.concat(Arg1, (Arg2 === null ? undefined : Arg2)); return result };
export const consumeVariant: (_:Component2variant) => number = Component1BS.consumeVariant;
export const l: list<number> = Component1BS.l;
export const map: <T11519,T11517>(_:(_:T11519) => T11517, _:list<T11519>) => list<T11517> = Component1BS.map;

