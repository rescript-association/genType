/* @flow strict */

var Greeting = require("./Greeting.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {componentSpec as ReasonReactcomponentSpec} from '../../src/shims/ReactShim.shim.js';
import type {list} from '../../src/shims/ReasonPervasives.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
const concat = Greeting.concat;
const cons = Greeting.cons;
const empty = Greeting.empty;
export type Props = {|message:string, extraGreeting?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  Greeting.component,
  (function (jsProps: Props) {
     return Greeting.make(jsProps.message, jsProps.extraGreeting, jsProps.children);
  }));

exports.concat = (concat: (string, list<string>) => string);
exports.cons = (cons: <T7984>(T7984, list<T7984>) => list<T7984>);
exports.empty = (empty: list<string>);
exports.component = (component: React$ComponentType<Props>);