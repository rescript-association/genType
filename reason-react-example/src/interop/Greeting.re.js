/* @flow strict */

const Greeting = require("./Greeting.bs");
const ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {componentSpec as ReasonReactcomponentSpec} from '../../src/shims/ReactShim.shim.js';
import type {list} from '../../src/shims/ReasonPervasives.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
export type Props = {|message:string, someNumber:number, extraGreeting?:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  Greeting.component,
  (function (jsProps: Props) {
     return Greeting.make(jsProps.message, jsProps.someNumber, jsProps.extraGreeting, jsProps.children);
  }));
const empty = Greeting.empty;
const cons = (function (Arg1) { const result = Greeting.cons(Arg1.x, Arg1.l); return result });
const cons2 = (function (Arg1) { const result = Greeting.cons2(Arg1.l, Arg1.x); return result });
const concat = Greeting.concat;
const testNamedArgs = (function (Arg1, Arg2, Arg3, Arg4, Arg5) { const result = Greeting.testNamedArgs(Arg1.a, Arg1.b, Arg2, Arg3.c, Arg3.d, Arg4, Arg5.e); return result });
const testCallNamedArgs = (function (Arg1, Arg2, Arg3) { const result = Greeting.testCallNamedArgs((function (Arga, Argb) { const result = Arg1({a:Arga, b:Argb}); return result }), Arg2, Arg3); return result });
const testDefaultArgs = (function (Arg1) { const result = Greeting.testDefaultArgs(Arg1.x, Arg1.y); return result });

exports.component = (component: React$ComponentType<Props>);
exports.empty = (empty: list<string>);
exports.cons = (cons: <T8013>({|x:T8013, l:list<T8013>|}) => list<T8013>);
exports.cons2 = (cons2: <T8031>({|l:list<T8031>, x:T8031|}) => list<T8031>);
exports.concat = (concat: (string, list<string>) => string);
exports.testNamedArgs = (testNamedArgs: ({|a:number, b:number|}, number, {|c:number, d:number|}, number, {|e:number|}) => number);
exports.testCallNamedArgs = (testCallNamedArgs: (({|a:number, b:number|}) => number, number, number) => number);
exports.testDefaultArgs = (testDefaultArgs: ({|x?:number, y:number|}) => number);