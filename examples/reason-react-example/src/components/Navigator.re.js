/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const Curry = require('bs-platform/lib/js/curry.js');

// $FlowExpectedError: Reason checked type sufficiently
const NavigatorBS = require('./Navigator.bs');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type Design1_functionTypeWithGenTypeAs = ({|+type: string, +$number: number|}) => number;

export type Props = {|
  +history: mixed, 
  +match: mixed, 
  +children?: mixed
|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  NavigatorBS.component,
  (function _(jsProps: Props) {
     return Curry._3(NavigatorBS.make, jsProps.history, jsProps.match, jsProps.children);
  }));

export default component;

export type Design1_Props = {|
  +type_: mixed, 
  +number: mixed, 
  +children?: mixed
|};

export const Design1: React$ComponentType<Design1_Props> = ReasonReact.wrapReasonForJs(
  NavigatorBS.component,
  (function _(jsProps: Design1_Props) {
     return Curry._3(NavigatorBS.make, jsProps.type_, jsProps.number, jsProps.children);
  }));
