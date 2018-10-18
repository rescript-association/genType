/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const ManyComponentsBS = require('./ManyComponents.bs');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type InnerComponent_Props = {|children?: mixed|};

export const InnerComponent: React$ComponentType<InnerComponent_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: InnerComponent_Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export type Props = {|children?: mixed|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export default component;
