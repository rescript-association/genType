/* @flow strict */

var GreetingRe = require("./GreetingRe.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {componentSpec as ReasonReactcomponentSpec} from '../../src/shims/ReactShim.shim.js';
import type {list as list} from '../../src/shims/ReasonPervasives.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
const concat = GreetingRe.concat;
const cons = GreetingRe.cons;
const empty = GreetingRe.empty;
export type Props = {|message:string, extraGreeting?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  GreetingRe.component,
  (function (jsProps: Props) {
     return GreetingRe.make(jsProps.message, jsProps.extraGreeting, jsProps.children);
  }));

exports.concat = (concat: (string, list<string>) => string);
exports.cons = (cons: <T7984>(T7984, list<T7984>) => list<T7984>);
exports.empty = (empty: list<string>);
exports.component = (component: React$ComponentType<Props>);