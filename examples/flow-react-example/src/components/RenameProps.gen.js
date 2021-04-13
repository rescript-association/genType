/** 
 * @flow strict
 * @generated from RenameProps.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as RenamePropsBS from './RenameProps.bs';

export type functionTypeWithGenTypeAs = ({| +type: string, +$number: number |}) => number;

export const functionWithGenTypeAs: ({|
  +firstNameArgumentCantBeRenamed: string, 
  +type: string, 
  +$$number: number
|}) => string = function (Arg1: $any) {
  const result = Curry._3(RenamePropsBS.functionWithGenTypeAs, Arg1.firstNameArgumentCantBeRenamed, Arg1.type, Arg1.$$number);
  return result
};

export type Props = {|
  +firstNameArgumentCantBeRenamed: string, 
  +type: string, 
  +$$number: number, 
  +children?: mixed
|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  RenamePropsBS.component,
  (function _(jsProps: Props) {
     return Curry._4(RenamePropsBS.make, jsProps.firstNameArgumentCantBeRenamed, jsProps.type, jsProps.$$number, jsProps.children);
  }));

export default component;

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
