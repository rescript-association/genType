/* @flow strict */

var GreetingRe = require("./GreetingRe.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../shims/ReasonReactShim';
import type {componentSpec as ReasonReactcomponentSpec} from '../shims/ReasonReactShim';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../shims/ReasonReactShim';
import type {stateless as ReasonReactstateless} from '../shims/ReasonReactShim';
export type Props = {|message:string, extraGreeting?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  GreetingRe.component,
  (function (jsProps: Props) {
     return GreetingRe.make(jsProps.message, jsProps.extraGreeting, jsProps.children);
  }));

exports.component = (component: React$ComponentType<Props>);