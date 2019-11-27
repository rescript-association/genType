/* TypeScript file generated from ManyComponents.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const ManyComponentsBS = require('./ManyComponents.bs');

// tslint:disable-next-line:no-var-requires
const ReasonReact = require('reason-react/src/ReasonReact.js');

// tslint:disable-next-line:interface-over-type-literal
export type InnerComponent_Props = { readonly children?: unknown };

export const InnerComponent: React.ComponentType<InnerComponent_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.InnerComponent.component,
  (function _(jsProps: InnerComponent_Props) {
     return ManyComponentsBS.InnerComponent.make(jsProps.children);
  }));

// tslint:disable-next-line:interface-over-type-literal
export type ManyProps_Props = {
  readonly a: unknown; 
  readonly b: unknown; 
  readonly c: unknown; 
  readonly d: unknown; 
  readonly e: unknown; 
  readonly f: unknown; 
  readonly g: unknown; 
  readonly h: unknown; 
  readonly children?: unknown
};

export const ManyProps: React.ComponentType<ManyProps_Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.ManyProps.component,
  (function _(jsProps: ManyProps_Props) {
     return Curry.app(ManyComponentsBS.ManyProps.make, [jsProps.a, jsProps.b, jsProps.c, jsProps.d, jsProps.e, jsProps.f, jsProps.g, jsProps.h, jsProps.children]);
  }));

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly children?: unknown };

export const ManyComponents: React.ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ManyComponentsBS.component,
  (function _(jsProps: Props) {
     return ManyComponentsBS.make(jsProps.children);
  }));

export default ManyComponents;
