/** 
 * @flow strict
 * @generated from RenameProps.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError[unclear-type]: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as RenamePropsBS from './RenameProps.bs';

// flowlint-next-line nonstrict-import:off
import type {actionless as ReasonReact_actionless} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {componentSpec as ReasonReact_componentSpec} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {noRetainedProps as ReasonReact_noRetainedProps} from '../../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {stateless as ReasonReact_stateless} from '../../src/shims/ReactShim.shim';

export type functionTypeWithGenTypeAs = ({| +type: string, +"$number": number |}) => number;

export const functionWithGenTypeAs: ({|
  +firstNameArgumentCantBeRenamed: string, 
  +type: string, 
  +"$$number": number
|}) => string = function (Arg1: $any) {
  const result = Curry._3(RenamePropsBS.functionWithGenTypeAs, Arg1.firstNameArgumentCantBeRenamed, Arg1.type, Arg1.$$number);
  return result
};

export const make: <T1>({|
  +firstNameArgumentCantBeRenamed: string, 
  +type: string, 
  +"$$number": number
|}, T1) => ReasonReact_componentSpec<ReasonReact_stateless,ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_noRetainedProps,ReasonReact_actionless> = function <T1>(Arg1: $any, Arg2: $any) {
  const result = Curry._4(RenamePropsBS.make, Arg1.firstNameArgumentCantBeRenamed, Arg1.type, Arg1.$$number, Arg2);
  return result
};

export const firstIsIgnored: ({| +Ignored: number |}) => number = function (Arg1: $any) {
  const result = RenamePropsBS.firstIsIgnored(Arg1.Ignored);
  return result
};

export const padding1: (number, {| +xRenamed: number |}) => number = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(RenamePropsBS.padding1, Arg1, Arg2.xRenamed);
  return result
};

export const padding2: ({| +pad: number, +xRenamed: number |}) => number = function (Arg1: $any) {
  const result = Curry._2(RenamePropsBS.padding2, Arg1.pad, Arg1.xRenamed);
  return result
};

export const padding3: (number, number, {| +xRenamed: number |}) => number = function (Arg1: $any, Arg2: $any, Arg3: $any) {
  const result = Curry._3(RenamePropsBS.padding3, Arg1, Arg2, Arg3.xRenamed);
  return result
};

export const renameABunch: (number, {|
  +xRenamed: number, 
  +yRenamed: number, 
  +zRenamed: number
|}) => number = function (Arg1: $any, Arg2: $any) {
  const result = Curry._4(RenamePropsBS.renameABunch, Arg1, Arg2.xRenamed, Arg2.yRenamed, Arg2.zRenamed);
  return result
};

export const renameABunch2: (number, {|
  +xRenamed: number, 
  +y: number, 
  +zRenamed: number
|}) => number = function (Arg1: $any, Arg2: $any) {
  const result = Curry._4(RenamePropsBS.renameABunch2, Arg1, Arg2.xRenamed, Arg2.y, Arg2.zRenamed);
  return result
};

export const renameABunch3: (number, {| +xRenamed: number |}, number, {| +zRenamed: number |}) => number = function (Arg1: $any, Arg2: $any, Arg3: $any, Arg4: $any) {
  const result = Curry._4(RenamePropsBS.renameABunch3, Arg1, Arg2.xRenamed, Arg3, Arg4.zRenamed);
  return result
};
