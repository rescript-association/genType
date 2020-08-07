/* TypeScript file generated from ComponentAsProp.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const ComponentAsPropBS = require('./ComponentAsProp.bs');

// tslint:disable-next-line:no-var-requires
const ReasonReact = require('reason-react/src/ReasonReact.js');

// tslint:disable-next-line:interface-over-type-literal
export type Props = {
  readonly title: JSX.Element; 
  readonly description: JSX.Element; 
  readonly button?: JSX.Element; 
  readonly children?: unknown
};

export const ComponentAsProp: React.ComponentType<Props> = ReasonReact.wrapReasonForJs(
  ComponentAsPropBS.component,
  (function _(jsProps: Props) {
     return Curry._4(ComponentAsPropBS.make, jsProps.title, jsProps.description, jsProps.button, jsProps.children);
  }));

export default ComponentAsProp;
