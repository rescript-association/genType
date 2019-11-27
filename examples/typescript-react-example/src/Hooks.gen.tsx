/* TypeScript file generated from Hooks.re by genType. */
/* eslint-disable import/first */


import * as React from 'react';

// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const HooksBS = require('./Hooks.bs');

// tslint:disable-next-line:interface-over-type-literal
export type vehicle = { readonly name: string };

// tslint:disable-next-line:interface-over-type-literal
export type cb = (_1:{ readonly to: vehicle }) => void;

// tslint:disable-next-line:interface-over-type-literal
export type r = { readonly x: string };

// tslint:disable-next-line:interface-over-type-literal
export type callback<input,output> = (_1:input) => output;

// tslint:disable-next-line:interface-over-type-literal
export type testReactContext = React.Context<number>;

// tslint:disable-next-line:interface-over-type-literal
export type testReactRef = { current: (null | number) };

// tslint:disable-next-line:interface-over-type-literal
export type testDomRef = React.Ref<unknown>;

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly vehicle: vehicle };

export const $$default: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.default, $props);
  return result
};

export default $$default;

// tslint:disable-next-line:interface-over-type-literal
export type anotherComponent_Props = { readonly callback: (_1:void) => void; readonly vehicle: vehicle };

export const anotherComponent: React.ComponentType<{ readonly callback: (_1:void) => void; readonly vehicle: vehicle }> = function Hooks_anotherComponent(Arg1: any) {
  const $props = {callback:Arg1.callback, vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.anotherComponent, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type Inner_make_Props = { readonly vehicle: vehicle };

export const Inner_make: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks_Inner(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.Inner.make, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type Inner_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks_Inner_anotherComponent(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.Inner.anotherComponent, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_make_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_make: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks_Inner_Inner2(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.Inner.Inner2.make, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks_Inner_Inner2_anotherComponent(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.Inner.Inner2.anotherComponent, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type NoProps_make_Props = {};

export const NoProps_make: React.ComponentType<{}> = HooksBS.NoProps.make;

export const functionWithRenamedArgs: (_1:{
  readonly to: vehicle; 
  readonly Type: vehicle; 
  readonly cb: cb
}) => string = function (Arg1: any) {
  const result = Curry._3(HooksBS.functionWithRenamedArgs, [Arg1.to.name], [Arg1.Type.name], function (Argto: any) {
      const result1 = Arg1.cb({to:{name:Argto[0]}});
      return result1
    });
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type componentWithRenamedArgs_Props = {
  readonly Type: vehicle; 
  readonly to: vehicle; 
  readonly cb: cb
};

export const componentWithRenamedArgs: React.ComponentType<{
  readonly Type: vehicle; 
  readonly to: vehicle; 
  readonly cb: cb
}> = function Hooks_componentWithRenamedArgs(Arg1: any) {
  const $props = {Type:[Arg1.Type.name], to:[Arg1.to.name], cb:function (Argto: any) {
      const result1 = Arg1.cb({to:{name:Argto[0]}});
      return result1
    }};
  const result = React.createElement(HooksBS.componentWithRenamedArgs, $props);
  return result
};

export const makeWithRef: (_1:{ readonly vehicle: vehicle }, _2:(null | undefined | any)) => JSX.Element = function (Arg1: any, Arg2: any) {
  const result = Curry._2(HooksBS.makeWithRef, {vehicle:[Arg1.vehicle.name]}, Arg2);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type testForwardRef_Props = { readonly vehicle: vehicle };

export const testForwardRef: React.ComponentType<{ readonly vehicle: vehicle }> = function Hooks_testForwardRef(Arg1: any) {
  const $props = {vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.testForwardRef, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type input_Props = { readonly r: r };

export const input: React.ComponentType<{ readonly r: r }> = function Hooks_input(Arg1: any) {
  const $props = {r:[Arg1.r.x]};
  const result = React.createElement(HooksBS.input, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type polymorphicComponent_Props<T1> = { readonly p: [vehicle, T1] };

export const polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> = function Hooks_polymorphicComponent<T1>(Arg1: any) {
  const $props = {p:[[Arg1.p[0].name], Arg1.p[1]]};
  const result = React.createElement(HooksBS.polymorphicComponent, $props);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type functionReturningReactElement_Props = { readonly name: string };

export const functionReturningReactElement: React.ComponentType<{ readonly name: string }> = HooksBS.functionReturningReactElement;

// tslint:disable-next-line:interface-over-type-literal
export type RenderPropRequiresConversion_make_Props = { readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> };

export const RenderPropRequiresConversion_make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> = function Hooks_RenderPropRequiresConversion(Arg1: any) {
  const $props1 = {renderVehicle:function (Arg11: any) {
      const $props = {number:Arg11.number, vehicle:{name:Arg11.vehicle[0]}};
      const result1 = React.createElement(Arg1.renderVehicle, $props);
      return result1
    }};
  const result = React.createElement(HooksBS.RenderPropRequiresConversion.make, $props1);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type aComponentWithChildren_Props = { readonly children: React.ReactNode; readonly vehicle: vehicle };

export const aComponentWithChildren: React.ComponentType<{ readonly children: React.ReactNode; readonly vehicle: vehicle }> = function Hooks_aComponentWithChildren(Arg1: any) {
  const $props = {children:Arg1.children, vehicle:[Arg1.vehicle.name]};
  const result = React.createElement(HooksBS.aComponentWithChildren, $props);
  return result
};

export const NoProps: { make: React.ComponentType<{}> } = HooksBS.NoProps

export const Inner: {
  Inner2: {
    anotherComponent: React.ComponentType<{
      readonly vehicle: vehicle
    }>; 
    make: React.ComponentType<{
      readonly vehicle: vehicle
    }>
  }; 
  anotherComponent: React.ComponentType<{
    readonly vehicle: vehicle
  }>; 
  make: React.ComponentType<{
    readonly vehicle: vehicle
  }>
} = HooksBS.Inner

export const RenderPropRequiresConversion: { make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> } = HooksBS.RenderPropRequiresConversion
