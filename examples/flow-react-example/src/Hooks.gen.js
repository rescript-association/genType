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

export const $$default: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks(Arg1: $any) {
  const result = HooksBS.default({vehicle:[Arg1.vehicle.name]});
  return result
};

export default $$default;

export const anotherComponent: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks_anotherComponent(Arg1: $any) {
  const result = HooksBS.anotherComponent({vehicle:[Arg1.vehicle.name]});
  return result
};

export const Inner_make: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks_Inner(Arg1: $any) {
  const result = HooksBS.Inner[0]({vehicle:[Arg1.vehicle.name]});
  return result
};

export const Inner_anotherComponent: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks_Inner_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[1]({vehicle:[Arg1.vehicle.name]});
  return result
};

export const Inner_Inner2_make: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks_Inner_Inner2(Arg1: $any) {
  const result = HooksBS.Inner[2][0]({vehicle:[Arg1.vehicle.name]});
  return result
};

export const Inner_Inner2_anotherComponent: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function Hooks_Inner_Inner2_anotherComponent(Arg1: $any) {
  const result = HooksBS.Inner[2][1]({vehicle:[Arg1.vehicle.name]});
  return result
};
