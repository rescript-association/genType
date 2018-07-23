/* @flow strict */

var Component1 = require("../Component1.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {ComponentSpec as ReasonReactComponentSpec} from '../shims/ReasonReactFlowShim';
import type {Stateless as ReasonReactStateless} from '../shims/ReasonReactFlowShim';
import type {NoRetainedProps as ReasonReactNoRetainedProps} from '../shims/ReasonReactFlowShim';
import type {Actionless as ReasonReactActionless} from '../shims/ReasonReactFlowShim';
import type {Component as ReactComponent} from 'React';
const plus = Component1.plus;
export type Props = {|message?:string|};
const component = ReasonReact.wrapReasonForJs(
  Component1.component,
  (function (jsProps: {...Props, children:any}) {
     return Component1.make(/* TODO converter: optionalArgument(id) */ /* TODO: OptLabel*/jsProps.message, jsProps.children);
  }));

exports.plus = (plus: <T10970>(number, T10970) => number);
exports.component = (component: React$ComponentType<Props>);