/** 
 * @flow strict
 * @generated from Navigator.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as NavigatorBS from './Navigator.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

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
