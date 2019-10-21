/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
const Curry = require('bs-platform/lib/js/curry.js');

// $FlowExpectedError: Reason checked type sufficiently
const ManyComponentsBS = require('./ManyComponents.bs');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type InnerComponent_Props = {| +children?: mixed |};

const InnerComponent: React$ComponentType<InnerComponent_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.InnerComponent.component,
  (function _(jsProps: InnerComponent_Props) {
     return ManyComponentsBS.InnerComponent.make(jsProps.children);
  }));;
exports.InnerComponent = InnerComponent

export type ManyProps_Props = {|
  +a: mixed, 
  +b: mixed, 
  +c: mixed, 
  +d: mixed, 
  +e: mixed, 
  +f: mixed, 
  +g: mixed, 
  +h: mixed, 
  +children?: mixed
|};

const ManyProps: React$ComponentType<ManyProps_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.ManyProps.component,
  (function _(jsProps: ManyProps_Props) {
     return Curry.app(ManyComponentsBS.ManyProps.make, [jsProps.a, jsProps.b, jsProps.c, jsProps.d, jsProps.e, jsProps.f, jsProps.g, jsProps.h, jsProps.children]);
  }));;
exports.ManyProps = ManyProps

export type Props = {| +children?: mixed |};

const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));;
exports.component = component

exports.default = component;
