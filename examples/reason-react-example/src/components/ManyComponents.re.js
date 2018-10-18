/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const ManyComponentsBS = require('./ManyComponents.bs');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type Props = {|children?: mixed|};

export const InnerComponent: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export type Props2 = {|children?: mixed|};

export const component: React$ComponentType<Props2> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props2) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export default component;
