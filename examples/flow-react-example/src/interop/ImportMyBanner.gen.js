/** 
 * @flow strict
 * @generated from ImportMyBanner.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import MyBanner from './MyBanner.component';

// $FlowExpectedError: Reason checked type sufficiently
import * as React from 'react';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

export type Props = {| +show: boolean, +Message: ?string |};

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and the props of './MyBanner.component'.
export function MyBannerTypeChecked(props: Props): React$Node {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function (show: $any, Message: $any, children: $any) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, Message: Message}, children); };
