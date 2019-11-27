/** 
 * @flow strict
 * @generated from ComponentAsProp.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ComponentAsPropBS from './ComponentAsProp.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type Props = {|
  +title: React$Node, 
  +description: React$Node, 
  +button?: React$Node, 
  +children?: mixed
|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ComponentAsPropBS.component,
  (function _(jsProps: Props) {
     return Curry._4(ComponentAsPropBS.make, jsProps.title, jsProps.description, jsProps.button, jsProps.children);
  }));

export default component;
