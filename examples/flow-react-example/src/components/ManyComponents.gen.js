/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

const $$toRE312434514 = {"Small": 0, "Medium": 1, "Large": 2};

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ManyComponentsBS from './ManyComponents.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type size = "Small" | "Medium" | "Large";

export type InnerComponent_Props = {| +children?: mixed |};

export const InnerComponent: React$ComponentType<InnerComponent_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.InnerComponent.component,
  (function _(jsProps: InnerComponent_Props) {
     return ManyComponentsBS.InnerComponent.make(jsProps.children);
  }));

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

export const ManyProps: React$ComponentType<ManyProps_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.ManyProps.component,
  (function _(jsProps: ManyProps_Props) {
     return Curry.app(ManyComponentsBS.ManyProps.make, [jsProps.a, jsProps.b, jsProps.c, jsProps.d, jsProps.e, jsProps.f, jsProps.g, jsProps.h, jsProps.children]);
  }));

export type Props = {| +size?: size, +children?: mixed |};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return Curry._2(ManyComponentsBS.make, (jsProps.size == null ? undefined : $$toRE312434514[jsProps.size]), jsProps.children);
  }));

export default component;
