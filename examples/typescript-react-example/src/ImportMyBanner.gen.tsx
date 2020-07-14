/* TypeScript file generated from ImportMyBanner.res by genType. */
/* eslint-disable import/first */


import MyBanner from './MyBanner';

import * as React from 'react';

// tslint:disable-next-line:no-var-requires
const ReasonReact = require('reason-react/src/ReasonReact.js');

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly show: boolean; readonly message?: message };

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and the props of './MyBanner'.
export function MyBannerTypeChecked(props: Props): JSX.Element {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function (show: any, message: any, children: any) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, message: message}, children); };

// tslint:disable-next-line:interface-over-type-literal
export type message = { readonly text: string };
