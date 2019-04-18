/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as HooksBS from './Hooks.bs';

// flowlint-next-line nonstrict-import:off
import type {reactElement as ReasonReact_reactElement} from '../src/shims/ReactShim.shim';

export type vehicle = {| +name: string |};

// Type annotated function components are not checked by Flow, but typeof() works.
const $$default$$forTypeof = function (_: {| +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const $$default: typeof($$default$$forTypeof) = function Hooks(Arg1: $any) {
  const result = HooksBS.default({vehicle:[Arg1.vehicle.name]});
  return result
};

export default $$default;

// Type annotated function components are not checked by Flow, but typeof() works.
const anotherComponent$$forTypeof = function (_: {| +callback: (void) => void, +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const anotherComponent: typeof(anotherComponent$$forTypeof) = function Hooks_anotherComponent(Arg1: $any) {
  const result = HooksBS.anotherComponent({callback:function (Arg11: $any) {
      const result1 = Arg1.callback(Arg11);
      return result1
    }, vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const Inner_make: typeof(Inner_make$$forTypeof) = function Hooks_Inner(Arg1: $any) {
  const result = HooksBS.Inner[0]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const Inner_anotherComponent: typeof(Inner_anotherComponent$$forTypeof) = function Hooks_Inner_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[1]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_make$$forTypeof = function (_: {| +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const Inner_Inner2_make: typeof(Inner_Inner2_make$$forTypeof) = function Hooks_Inner_Inner2(Arg1: $any) {
  const result = HooksBS.Inner[2][0]({vehicle:[Arg1.vehicle.name]});
  return result
};

// Type annotated function components are not checked by Flow, but typeof() works.
const Inner_Inner2_anotherComponent$$forTypeof = function (_: {| +vehicle: vehicle |}) : ReasonReact_reactElement { return null };

export const Inner_Inner2_anotherComponent: typeof(Inner_Inner2_anotherComponent$$forTypeof) = function Hooks_Inner_Inner2_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[2][1]({vehicle:[Arg1.vehicle.name]});
  return result
};
