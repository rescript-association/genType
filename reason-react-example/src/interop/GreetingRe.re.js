/* @flow strict */

var GreetingRe = require("./GreetingRe.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {componentSpec as ReasonReactcomponentSpec} from '../../src/shims/ReactShim.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
export type Props = {|message:string, extraGreeting?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  GreetingRe.component,
  (function (jsProps: Props) {
     return GreetingRe.make(jsProps.message, jsProps.extraGreeting, jsProps.children);
  }));

exports.component = (component: React$ComponentType<Props>);