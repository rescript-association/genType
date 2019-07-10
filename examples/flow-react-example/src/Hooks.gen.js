/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as HooksBS from './Hooks.bs';

export type vehicle = {| +name: string |};

export type cb = ({| +to: vehicle |}) => void;

export type callback<input,output> = (input) => output;

export type testReactContext = React$Context<number>;

export type testReactRef = React$Ref<number>;

// Type annotated function components are not checked by Flow, but typeof() works.
const $$default$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const $$default: typeof($$default$$forTypeof) = function Hooks(Arg1: $any) {
  const result = HooksBS.default({vehicle:[Arg1.vehicle.name]});
  return result
};

export default $$default;

// Type annotated function components are not checked by Flow, but typeof() works.
const anotherComponent$$forTypeof = function (_: {| +callback: ((void) => void), +vehicle: vehicle |}) : React$Node { return null };

export const anotherComponent: typeof(anotherComponent$$forTypeof) = function Hooks_anotherComponent(Arg1: $any) {
  const result = HooksBS.anotherComponent({callback:Arg1.callback, vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const Inner_make: typeof(Inner_make$$forTypeof) = function Hooks_Inner(Arg1: $any) {
  const result = HooksBS.Inner[0]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const Inner_anotherComponent: typeof(Inner_anotherComponent$$forTypeof) = function Hooks_Inner_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[1]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const Inner_Inner2_make: typeof(Inner_Inner2_make$$forTypeof) = function Hooks_Inner_Inner2(Arg1: $any) {
  const result = HooksBS.Inner[2][0]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const Inner_Inner2_anotherComponent: typeof(Inner_Inner2_anotherComponent$$forTypeof) = function Hooks_Inner_Inner2_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[2][1]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const NoProps_make$$forTypeof = function (_: {||}) : React$Node { return null };

export const NoProps_make: typeof(NoProps_make$$forTypeof) = HooksBS.NoProps[0];

export const functionWithRenamedArgs: ({|
  +to: vehicle, 
  +Type: vehicle, 
  +cb: cb
|}) => string = function (Arg1: $any) {
  const result = Curry._3(HooksBS.functionWithRenamedArgs, [Arg1.to.name], [Arg1.Type.name], function (Argto: $any) {
      const result1 = Arg1.cb({to:{name:Argto[0]}});
      return result1
    });
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const componentWithRenamedArgs$$forTypeof = function (_: {|
  +Type: vehicle, 
  +to: vehicle, 
  +cb: cb
|}) : React$Node { return null };

export const componentWithRenamedArgs: typeof(componentWithRenamedArgs$$forTypeof) = function Hooks_componentWithRenamedArgs(Arg1: $any) {
  const result = HooksBS.componentWithRenamedArgs({Type:[Arg1.Type.name], to:[Arg1.to.name], cb:function (Argto: $any) {
      const result1 = Arg1.cb({to:{name:Argto[0]}});
      return result1
    }});
  return result
};

export const makeWithRef: ({| +vehicle: vehicle |}, ?$any) => React$Node = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(HooksBS.makeWithRef, {vehicle:[Arg1.vehicle.name]}, Arg2);
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const testForwardRef$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export const testForwardRef: typeof(testForwardRef$$forTypeof) = function Hooks_testForwardRef(Arg1: $any) {
  const result = HooksBS.testForwardRef({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const polymorphicComponent$$forTypeof = function <T1>(_: {| +p: [vehicle, T1] |}) : React$Node { return null };

export const polymorphicComponent: typeof(polymorphicComponent$$forTypeof) = function Hooks_polymorphicComponent<T1>(Arg1: $any) {
  const result = HooksBS.polymorphicComponent({p:[[Arg1.p[0].name], Arg1.p[1]]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const functionReturningReactElement$$forTypeof = function (_: {| +name: string |}) : React$Node { return null };

export const functionReturningReactElement: typeof(functionReturningReactElement$$forTypeof) = HooksBS.functionReturningReactElement;
