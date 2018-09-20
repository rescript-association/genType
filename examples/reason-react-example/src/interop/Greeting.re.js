/* @flow strict */

const GreetingBS = require("./Greeting.bs");
const ReasonReact = require("reason-react/src/ReasonReact.js");

import type {list} from '../../src/shims/ReasonPervasives.shim';

export type Props = {|message:string, someNumber:number, extraGreeting?:string, polymorphicProp:any, children?:any|};
export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  GreetingBS.component,
  (function _(jsProps: Props) {
     return GreetingBS.make(jsProps.message, jsProps.someNumber, jsProps.extraGreeting, jsProps.polymorphicProp, jsProps.children);
  }));
export default component;
export const empty: list<string> = GreetingBS.empty;
export const cons: <T1>({|x:T1, l:list<T1>|}) => list<T1> = function _(Arg1) { const result = GreetingBS.cons(Arg1.x, Arg1.l); return result };
export const cons2: <T1>({|l:list<T1>, x:T1|}) => list<T1> = function _(Arg1) { const result = GreetingBS.cons2(Arg1.l, Arg1.x); return result };
export const concat: (string, list<string>) => string = GreetingBS.concat;
export const testNamedArgs: ({|a:number, b:number|}, number, {|c:number, d:number|}, number, {|e:number|}) => number = function _(Arg1, Arg2, Arg3, Arg4, Arg5) { const result = GreetingBS.testNamedArgs(Arg1.a, Arg1.b, Arg2, Arg3.c, Arg3.d, Arg4, Arg5.e); return result };
export const testCallNamedArgs: (({|a:number, b:number|}) => number, number, number) => number = function _(Arg1, Arg2, Arg3) { const result = GreetingBS.testCallNamedArgs(function _(Arga, Argb) { const result = Arg1({a:Arga, b:Argb}); return result }, Arg2, Arg3); return result };
export const testDefaultArgs: ({|x?:number, y:number|}) => number = function _(Arg1) { const result = GreetingBS.testDefaultArgs(Arg1.x, Arg1.y); return result };