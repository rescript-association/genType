/* @flow strict */

var Component1 = require("./Component1.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../src/shims/ReactShim.shim.js';
import type {componentSpec as ReasonReactcomponentSpec} from '../src/shims/ReactShim.shim.js';
import type {list as list} from '../src/shims/ReasonPervasives.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../src/shims/ReactShim.shim.js';
import type {variant as Component2variant} from './Component2.re';
const map = Component1.map;
const l = Component1.l;
const consumeVariant = Component1.consumeVariant;
const concat = (function (Arg1, Arg2) { const result = Component1.concat(Arg1, (Arg2 === null ? undefined : Arg2)); return result });
const plus = Component1.plus;
export type Props = {|message?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  Component1.component,
  (function (jsProps: Props) {
     return Component1.make(jsProps.message, jsProps.children);
  }));

exports.map = (map: <T11480,T11478>((T11480) => T11478, list<T11480>) => list<T11478>);
exports.l = (l: list<number>);
exports.consumeVariant = (consumeVariant: (Component2variant) => number);
exports.concat = (concat: (string, ?string) => ?string);
exports.plus = (plus: <T10970>(number, T10970) => number);
exports.component = (component: React$ComponentType<Props>);