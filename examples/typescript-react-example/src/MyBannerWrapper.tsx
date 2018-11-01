/* TypeScript file generated by genType. */

import MyBanner from'./MyBanner';

import * as React from 'react';

// tslint:disable-next-line:no-var-requires
const ReasonReact = require('reason-react/src/ReasonReact.js');

// tslint:disable-next-line:interface-over-type-literal
export type Props = {readonly show: boolean, readonly message?: Imessage};

// In case of type error, check the type of 'make' in 'MyBannerWrapper.re' and the props of './MyBanner'.
export function MyBannerTypeChecked(props: Props) {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function _(show: any, message: any, children: any) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, message: (message == null ? message : {text:message[0]})}, children); };

export interface Imessage {readonly text: string};
