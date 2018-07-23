/* @flow strict */

var Component1 = require("../Component1.bs");
var ReasonReact = require("reason-react/src/ReasonReact.js");

import type {ComponentSpec as ReasonReactComponentSpec} from '../shims/ReasonReactFlowShim';
import type {Stateless as ReasonReactStateless} from '../shims/ReasonReactFlowShim';
import type {NoRetainedProps as ReasonReactNoRetainedProps} from '../shims/ReasonReactFlowShim';
import type {Actionless as ReasonReactActionless} from '../shims/ReasonReactFlowShim';
import type {Component as ReactComponent} from 'React';
// No need to import locally visible type TwoVariants. Make sure it is also marked with @genFlow;
const consumeTwoVariants = Component1.consumeTwoVariants;
export opaque type TwoVariantsA = any // Reason type already checked. Making it opaque;
// type __flowTypeValueAnnotation____capitalizeExport__a = TwoVariantsA
const A = null /*TODO*/;
// constructorAlias: __capitalizeExport__a
// convertableFlowTypes: 
// modulePath: Component1
// leafName: A
// TODO: ConstructorBinding
export opaque type TwoVariantsB = any // Reason type already checked. Making it opaque;
// type __flowTypeValueAnnotation____capitalizeExport__b = (number) => TwoVariantsB
const B = null /*TODO*/;
// constructorAlias: __capitalizeExport__b
// convertableFlowTypes: (id, number)
// modulePath: Component1
// leafName: B
// TODO: ConstructorBinding
export opaque type TwoVariantsC = any // Reason type already checked. Making it opaque;
// type __flowTypeValueAnnotation____capitalizeExport__c = (?number) => TwoVariantsC
const C = null /*TODO*/;
// constructorAlias: __capitalizeExport__c
// convertableFlowTypes: (option(id), ?number)
// modulePath: Component1
// leafName: C
// TODO: ConstructorBinding
export type TwoVariants =
  | TwoVariantsA
  | TwoVariantsB
  | TwoVariantsC;
const concat = (function (Arg1, Arg2) { const result = Component1.concat(Arg1, (Arg2 === null ? undefined : Arg2)); return result });
const plus = Component1.plus;
export type Props = {|message?:string|};
const component = ReasonReact.wrapReasonForJs(
  Component1.component,
  (function (jsProps: {...Props, children:any}) {
     return Component1.make(jsProps.message, jsProps.children);
  }));

exports.consumeTwoVariants = (consumeTwoVariants: (TwoVariants) => number);
exports.A = (A: TwoVariantsA);
exports.B = (B: (number) => TwoVariantsB);
exports.C = (C: (?number) => TwoVariantsC);
exports.concat = (concat: (string, ?string) => ?string);
exports.plus = (plus: <T10970>(number, T10970) => number);
exports.component = (component: React$ComponentType<Props>);