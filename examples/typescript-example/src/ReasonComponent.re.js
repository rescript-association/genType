/* @flow strict */

var CreateBucklescriptBlock = require("bs-platform/lib/js/block.js");
var ReasonComponent = require("./ReasonComponent.bs");

// No need to import locally visible type t. Make sure it is also marked with @genFlow;
import type {List as List} from 'ReasonPervasives.bs';
import type {actionless as ReasonReactactionless} from './ReasonReact.re';
import type {component as ReasonReactcomponent} from './ReasonReact.re';
import type {noRetainedProps as ReasonReactnoRetainedProps} from './ReasonReact.re';
import type {stateless as ReasonReactstateless} from './ReasonReact.re';
import type {t as Typest} from '../src/nested/Types.re';
const tToString = ReasonComponent.tToString;
export opaque type TA = any // Reason type already checked. Making it opaque;
const A = 0;
export opaque type TB = any // Reason type already checked. Making it opaque;
function B(Arg1) { CreateBucklescriptBlock.__(0, [Arg1]) }
export opaque type TC = any // Reason type already checked. Making it opaque;
function C(Arg1) { CreateBucklescriptBlock.__(1, [Arg1]) }
export type t =
  | TA
  | TB
  | TC;
const useTypeDefinedInAnotherModule = ReasonComponent.useTypeDefinedInAnotherModule;
const minus = (function (Argfirst, Argsecond) { const result = ReasonComponent.minus(Argfirst, Argsecond); return result });
const make = (function (Argmessage, ArgintList, Arg3) { const result = ReasonComponent.make(Argmessage, ArgintList, Arg3); return result });

exports.tToString = (tToString: (t) => string);
exports.A = (A: TA);
exports.B = (B: (number) => TB);
exports.C = (C: (string) => TC);
exports.useTypeDefinedInAnotherModule = (useTypeDefinedInAnotherModule: (Typest) => Typest);
exports.minus = (minus: ({|first?:number, second:number|}) => number);
exports.make = (make: <T1011>({|message?:string, intList?:list<number>|}, T1011) => ReasonReactcomponent<ReasonReactstateless,ReasonReactnoRetainedProps,ReasonReactactionless>);