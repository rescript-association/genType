/** 
 * @flow strict
 * @generated from Hooks.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError[unclear-type]: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as React from 'react';

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as HooksBS from './Hooks.bs';

export type vehicle = {| +name: string |};

export type cb = ({| +to: vehicle |}) => void;

export type r = {| +x: string |};

export type callback<input,output> = (input) => output;

export type testReactContext = React$Context<number>;

export type testReactRef = { current: (null | number), ... };

export type testDomRef = React$Ref<mixed>;

export type testDOMReft = React$Ref<mixed>;

export type notAFunctionComponent = ({ current: (null | number), ... }) => React$Node;

export type Props = {| +vehicle: vehicle |};

export const $$default: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.default;

export default $$default;

export type anotherComponent_Props = {| +callback: () => void, +vehicle: vehicle |};

export const anotherComponent: React$ComponentType<{| +callback: () => void, +vehicle: vehicle |}> = HooksBS.anotherComponent;

export type Inner_make_Props = {| +vehicle: vehicle |};

export const Inner_make: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.Inner.make;

export type Inner_anotherComponent_Props = {| +vehicle: vehicle |};

export const Inner_anotherComponent: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.Inner.anotherComponent;

export type Inner_Inner2_make_Props = {| +vehicle: vehicle |};

export const Inner_Inner2_make: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.Inner.Inner2.make;

export type Inner_Inner2_anotherComponent_Props = {| +vehicle: vehicle |};

export const Inner_Inner2_anotherComponent: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.Inner.Inner2.anotherComponent;

export type NoProps_make_Props = {||};

export const NoProps_make: React$ComponentType<{||}> = HooksBS.NoProps.make;

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

export type componentWithRenamedArgs_Props = {|
  +Type: vehicle, 
  +to: vehicle, 
  +cb: cb
|};

export const componentWithRenamedArgs: React$ComponentType<{|
  +Type: vehicle, 
  +to: vehicle, 
  +cb: cb
|}> = function Hooks_componentWithRenamedArgs(Arg1: $any) {
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

export type testForwardRef_Props = {| +vehicle: vehicle |};

export const testForwardRef: React$ComponentType<{| +vehicle: vehicle |}> = HooksBS.testForwardRef;

export type input_Props = {| +r: r |};

export const input: React$ComponentType<{| +r: r |}> = HooksBS.input;

export type polymorphicComponent_Props<T1> = {| +p: [vehicle, T1] |};

export const polymorphicComponent: React$ComponentType<{| +p: [vehicle, $any] |}> = HooksBS.polymorphicComponent;

export type functionReturningReactElement_Props = {| +name: string |};

export const functionReturningReactElement: React$ComponentType<{| +name: string |}> = HooksBS.functionReturningReactElement;

export type RenderPropRequiresConversion_make_Props = {| +renderVehicle: React$ComponentType<{| +number: number, +vehicle: vehicle |}> |};

export const RenderPropRequiresConversion_make: React$ComponentType<{| +renderVehicle: React$ComponentType<{| +number: number, +vehicle: vehicle |}> |}> = HooksBS.RenderPropRequiresConversion.make;

export type aComponentWithChildren_Props = {| +children: React$Node, +vehicle: vehicle |};

export const aComponentWithChildren: React$ComponentType<{| +children: React$Node, +vehicle: vehicle |}> = HooksBS.aComponentWithChildren;

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
