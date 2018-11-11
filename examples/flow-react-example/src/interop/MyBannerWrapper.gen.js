/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// flowlint-next-line nonstrict-import:off
import MyBanner from './MyBanner.component';

// $FlowExpectedError: Reason checked type sufficiently
import * as React from 'react';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type Props = {|+show: boolean, +Message: ?string|};

// In case of type error, check the type of 'make' in 'MyBannerWrapper.re' and the props of './MyBanner.component'.
export function MyBannerTypeChecked(props: Props) {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function _(show, Message, children) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, Message: Message}, children); };
