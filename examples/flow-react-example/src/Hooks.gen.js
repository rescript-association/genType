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

export const make: ({| +vehicle: vehicle |}) => ReasonReact_reactElement = function (Arg1: $any) {
  const result = HooksBS.make({vehicle:[Arg1.vehicle.name]});
  return result
};
