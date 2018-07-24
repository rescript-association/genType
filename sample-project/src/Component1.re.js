/* @flow strict */

var Component1 = require("./Component1.bs");

import type {componentSpec as ReasonReactcomponentSpec} from './shims/ReasonReactShim';
import type {stateless as ReasonReactstateless} from './shims/ReasonReactShim';
import type {noRetainedProps as ReasonReactnoRetainedProps} from './shims/ReasonReactShim';
import type {actionless as ReasonReactactionless} from './shims/ReasonReactShim';
import type {variant as Component2variant} from './Component2.re';
const consumeVariant = Component1.consumeVariant;
const concat = (function (Arg1, Arg2) { const result = Component1.concat(Arg1, (Arg2 === null ? undefined : Arg2)); return result });
const plus = Component1.plus;
const make = (function (Argmessage, Arg2) { const result = Component1.make(Argmessage, Arg2); return result });

exports.consumeVariant = (consumeVariant: (Component2variant) => number);
exports.concat = (concat: (string, ?string) => ?string);
exports.plus = (plus: <T10970>(number, T10970) => number);
exports.make = (make: <T1124>({|message?:string|}, T1124) => ReasonReactcomponentSpec<ReasonReactstateless,ReasonReactstateless,ReasonReactnoRetainedProps,ReasonReactnoRetainedProps,ReasonReactactionless>);