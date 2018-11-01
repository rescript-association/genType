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

export type Props = {
  +history: mixed, 
  +match: mixed, 
  +children?: mixed
};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  NavigatorBS.component,
  (function _(jsProps: Props) {
     return Curry._3(NavigatorBS.make, jsProps.history, jsProps.match, jsProps.children);
  }));

export default component;
