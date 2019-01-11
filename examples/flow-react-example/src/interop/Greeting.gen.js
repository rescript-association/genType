/** 
 * @flow strict
 * @generated
 * @nolint
 */

const $$toRE453167283 = {"A": 0, "B": 1};

const $$toJS453167283 = {"0": "A", "1": "B"};

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as GreetingBS from './Greeting.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// flowlint-next-line nonstrict-import:off
import type {Mouse_t as ReactEvent_Mouse_t} from '../../src/shims/ReactEvent.shim';

// flowlint-next-line nonstrict-import:off
import type {list} from '../../src/shims/ReasonPervasives.shim';

export type foo = ({| +a: number, +b: number |}) => number;

export type someRecord = {| +x: number, +y: string |};

export type someVariant = "A" | "B";

export const onClick: (ReactEvent_Mouse_t) => void = GreetingBS.onClick;

export type Props = {|
  +message: string, 
  +someNumber: number, 
  +extraGreeting?: string, 
  +polymorphicProp: mixed, 
  +children?: mixed
|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  GreetingBS.component,
  (function _(jsProps: Props) {
     return Curry._5(GreetingBS.make, jsProps.message, jsProps.someNumber, jsProps.extraGreeting, jsProps.polymorphicProp, jsProps.children);
  }));

export default component;

export const empty: list<string> = GreetingBS.empty;

export const cons: <T1>({| +x: T1, +l: list<T1> |}) => list<T1> = function _(Arg1) {
  const result = GreetingBS.cons(Arg1.x, Arg1.l);
  return result
};

export const cons2: <T1>({| +l: list<T1>, +x: T1 |}) => list<T1> = function _(Arg1) {
  const result = GreetingBS.cons2(Arg1.l, Arg1.x);
  return result
};

export const concat: (string, list<string>) => string = GreetingBS.concat;

export const testNamedArgs: ({| +a: number, +b: number |}, number, {| +c: number, +d: number |}, number, {| +e: number |}) => number = function _(Arg1, Arg2, Arg3, Arg4, Arg5) {
  const result = GreetingBS.testNamedArgs(Arg1.a, Arg1.b, Arg2, Arg3.c, Arg3.d, Arg4, Arg5.e);
  return result
};

export const testCallNamedArgs: (foo, number, number) => number = function _(Arg1, Arg2, Arg3) {
  const result = GreetingBS.testCallNamedArgs(function _(Arga, Argb) {
      const result1 = Arg1({a:Arga, b:Argb});
      return result1
    }, Arg2, Arg3);
  return result
};

export const testDefaultArgs: ({| +x?: number, +y: number |}) => number = function _(Arg1) {
  const result = GreetingBS.testDefaultArgs(Arg1.x, Arg1.y);
  return result
};

export const testDefaultArgsWithRecordConversion: ({| +size?: someRecord |}, void) => number = function _(Arg1, Arg2) {
  const result = GreetingBS.testDefaultArgsWithRecordConversion((Arg1.size == null ? undefined : [Arg1.size.x, Arg1.size.y]), Arg2);
  return result
};

export const testDefaultArgsWithVariantConversion: ({| +size?: someVariant |}, void) => number = function _(Arg1, Arg2) {
  const result = GreetingBS.testDefaultArgsWithVariantConversion((Arg1.size == null ? undefined : $$toRE453167283[Arg1.size]), Arg2);
  return result
};

export const testDefaultArgsWithVariantConversionContravariant: ((({| +size?: ?someVariant |}) => void), void) => number = function _(Arg1, Arg2) {
  const result = GreetingBS.testDefaultArgsWithVariantConversionContravariant(function _(Argsize) {
      const result1 = Arg1({size:(Argsize == null ? Argsize : $$toJS453167283[Argsize])});
      return result1
    }, Arg2);
  return result
};
