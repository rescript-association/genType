/** 
 * @flow strict
 * @generated from Hooks.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as React from 'react';

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as HooksBS from './Hooks.bs';

export type vehicle = {| +name: string |};

export type cb = ({| +to: vehicle |}) => void;

export type r = {| +x: string |};

export type callback<input,output> = (input) => output;

export type testReactContext = React$Context<number>;

export type testReactRef = { current: (null | number), ... };

export type testDomRef = React$Ref<mixed>;

export type notAFunctionComponent = ({ current: (null | number), ... }) => React$Node;

// Type annotated function components are not checked by Flow, but typeof() works.
const $$default$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Props = {| +vehicle: vehicle |};

export const $$default: typeof($$default$$forTypeof) = HooksBS.default;

export default $$default;

// Type annotated function components are not checked by Flow, but typeof() works.
const anotherComponent$$forTypeof = function (_: {| +callback: (() => void), +vehicle: vehicle |}) : React$Node { return null };

export type anotherComponent_Props = {| +callback: () => void, +vehicle: vehicle |};

export const anotherComponent: typeof(anotherComponent$$forTypeof) = HooksBS.anotherComponent;

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Inner_make_Props = {| +vehicle: vehicle |};

export const Inner_make: typeof(Inner_make$$forTypeof) = HooksBS.Inner.make;

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Inner_anotherComponent_Props = {| +vehicle: vehicle |};

export const Inner_anotherComponent: typeof(Inner_anotherComponent$$forTypeof) = HooksBS.Inner.anotherComponent;

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Inner_Inner2_make_Props = {| +vehicle: vehicle |};

export const Inner_Inner2_make: typeof(Inner_Inner2_make$$forTypeof) = HooksBS.Inner.Inner2.make;

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type Inner_Inner2_anotherComponent_Props = {| +vehicle: vehicle |};

export const Inner_Inner2_anotherComponent: typeof(Inner_Inner2_anotherComponent$$forTypeof) = HooksBS.Inner.Inner2.anotherComponent;

// Type annotated function components are not checked by Flow, but typeof() works.
const NoProps_make$$forTypeof = function (_: {||}) : React$Node { return null };

export type NoProps_make_Props = {||};

export const NoProps_make: typeof(NoProps_make$$forTypeof) = HooksBS.NoProps.make;

export const functionWithRenamedArgs: ({|
  +to: vehicle, 
  +Type: vehicle, 
  +cb: cb
|}) => string = function (Arg1: $any) {
  const result = Curry._3(HooksBS.functionWithRenamedArgs, Arg1.to, Arg1.Type, function (Argto: $any) {
      const result1 = Arg1.cb({to:Argto});
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

export type componentWithRenamedArgs_Props = {|
  +Type: vehicle, 
  +to: vehicle, 
  +cb: cb
|};

export const componentWithRenamedArgs: typeof(componentWithRenamedArgs$$forTypeof) = function Hooks_componentWithRenamedArgs(Arg1: $any) {
  const $props = {Type:Arg1.Type, to:Arg1.to, cb:function (Argto: $any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    }};
  const result = React.createElement(HooksBS.componentWithRenamedArgs, $props);
  return result
};

export const makeWithRef: ({| +vehicle: vehicle |}, ?$any) => React$Node = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(HooksBS.makeWithRef, Arg1, Arg2);
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const testForwardRef$$forTypeof = function (_: {| +vehicle: vehicle |}) : React$Node { return null };

export type testForwardRef_Props = {| +vehicle: vehicle |};

export const testForwardRef: typeof(testForwardRef$$forTypeof) = HooksBS.testForwardRef;

// Type annotated function components are not checked by Flow, but typeof() works.
const input$$forTypeof = function (_: {| +r: r |}) : React$Node { return null };

export type input_Props = {| +r: r |};

export const input: typeof(input$$forTypeof) = HooksBS.input;

// Type annotated function components are not checked by Flow, but typeof() works.
const polymorphicComponent$$forTypeof = function <T1>(_: {| +p: [vehicle, T1] |}) : React$Node { return null };

export type polymorphicComponent_Props<T1> = {| +p: [vehicle, T1] |};

export const polymorphicComponent: typeof(polymorphicComponent$$forTypeof) = HooksBS.polymorphicComponent;

// Type annotated function components are not checked by Flow, but typeof() works.
const functionReturningReactElement$$forTypeof = function (_: {| +name: string |}) : React$Node { return null };

export type functionReturningReactElement_Props = {| +name: string |};

export const functionReturningReactElement: typeof(functionReturningReactElement$$forTypeof) = HooksBS.functionReturningReactElement;

// Type annotated function components are not checked by Flow, but typeof() works.
const RenderPropRequiresConversion_make$$forTypeof = function (_: {| +renderVehicle: React$ComponentType<{| +number: number, +vehicle: vehicle |}> |}) : React$Node { return null };

export type RenderPropRequiresConversion_make_Props = {| +renderVehicle: React$ComponentType<{| +number: number, +vehicle: vehicle |}> |};

export const RenderPropRequiresConversion_make: typeof(RenderPropRequiresConversion_make$$forTypeof) = HooksBS.RenderPropRequiresConversion.make;

// Type annotated function components are not checked by Flow, but typeof() works.
const aComponentWithChildren$$forTypeof = function (_: {| +children: React$Node, +vehicle: vehicle |}) : React$Node { return null };

export type aComponentWithChildren_Props = {| +children: React$Node, +vehicle: vehicle |};

export const aComponentWithChildren: typeof(aComponentWithChildren$$forTypeof) = HooksBS.aComponentWithChildren;

export const NoProps: { make: React$ComponentType<{||}>, ... } = HooksBS.NoProps

export const Inner: {
  Inner2: {
    anotherComponent: React$ComponentType<{|
      +vehicle: vehicle
    |}>, 
    make: React$ComponentType<{|
      +vehicle: vehicle
    |}>, 
    ...
  }, 
  anotherComponent: React$ComponentType<{|
    +vehicle: vehicle
  |}>, 
  make: React$ComponentType<{|
    +vehicle: vehicle
  |}>, 
  ...
} = HooksBS.Inner

export const RenderPropRequiresConversion: { make: React$ComponentType<{| +renderVehicle: React$ComponentType<{| +number: number, +vehicle: vehicle |}> |}>, ... } = HooksBS.RenderPropRequiresConversion
