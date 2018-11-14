/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/js/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as ManyComponentsBS from './ManyComponents.bs';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type InnerComponent_Props = {|+children?: mixed|};

export const InnerComponent: React$ComponentType<InnerComponent_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: InnerComponent_Props) {
     return ManyComponentsBS.make(jsProps.children);
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
  ManyComponentsBS.component,
  (function _(jsProps: ManyProps_Props) {
     return Curry.app(ManyComponentsBS.make, [jsProps.a, jsProps.b, jsProps.c, jsProps.d, jsProps.e, jsProps.f, jsProps.g, jsProps.h, jsProps.children]);
  }));

export type Props = {|+children?: mixed|};

export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export default component;
