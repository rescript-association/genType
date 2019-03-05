/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ComponentAsPropBS from './ComponentAsProp.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// flowlint-next-line nonstrict-import:off
import type {reactElement as ReasonReact_reactElement} from '../../src/shims/ReactShim.shim';

export type Props = {|
  +title: ReasonReact_reactElement, 
  +description: ReasonReact_reactElement, 
  +button?: ReasonReact_reactElement, 
  +children?: mixed
|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ComponentAsPropBS.component,
  (function _(jsProps: Props) {
     return Curry._4(ComponentAsPropBS.make, jsProps.title, jsProps.description, jsProps.button, jsProps.children);
  }));

export default component;
