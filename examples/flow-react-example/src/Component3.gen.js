/** 
 * @flow strict
 * @generated from Component3.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Component3BS from './Component3.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type Props = {| +children?: mixed |};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  Component3BS.component,
  (function _(jsProps: Props) {
     return Component3BS.make(jsProps.children);
  }));

export default component;
