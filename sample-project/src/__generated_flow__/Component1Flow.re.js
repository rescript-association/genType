/* @flow strict */

var Component1 = require("../Component1.bs");
var CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {ComponentSpec as ReasonReactComponentSpec} from '../shims/ReasonReactFlowShim';
import type {Stateless as ReasonReactStateless} from '../shims/ReasonReactFlowShim';
import type {NoRetainedProps as ReasonReactNoRetainedProps} from '../shims/ReasonReactFlowShim';
import type {Actionless as ReasonReactActionless} from '../shims/ReasonReactFlowShim';
import type {Component as ReactComponent} from 'React';
// No need to import locally visible type Variant. Make sure it is also marked with @genFlow;
const consumeVariant = Component1.consumeVariant;
export opaque type VariantA = any // Reason type already checked. Making it opaque;
const A = 0;
export opaque type VariantB = any // Reason type already checked. Making it opaque;
function B(Arg0, Arg1) { return CreateBucklescriptBlock.__(0, [Arg0, Arg1]) }
export opaque type VariantC = any // Reason type already checked. Making it opaque;
function C(Arg0) { return CreateBucklescriptBlock.__(1, [(Arg0 === null ? undefined : Arg0)]) }
export type Variant =
  | VariantA
  | VariantB
  | VariantC;
const concat = (function (Arg1, Arg2) { const result = Component1.concat(Arg1, (Arg2 === null ? undefined : Arg2)); return result });
const plus = Component1.plus;
export type Props = {|message?:string|};
const component = ReasonReact.wrapReasonForJs(
  Component1.component,
  (function (jsProps: {...Props, children:any}) {
     return Component1.make(jsProps.message, jsProps.children);
  }));

exports.consumeVariant = (consumeVariant: (Variant) => number);
exports.A = (A: VariantA);
exports.B = (B: (number, number) => VariantB);
exports.C = (C: (?number) => VariantC);
exports.concat = (concat: (string, ?string) => ?string);
exports.plus = (plus: <T10970>(number, T10970) => number);
exports.component = (component: React$ComponentType<Props>);