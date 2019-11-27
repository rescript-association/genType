/** 
 * @flow strict
 * @generated from ImportMyBanner.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
const React = require('react');

// flowlint-next-line nonstrict-import:off
const MyBanner = require('./MyBanner.component');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type Props = {| +show: boolean, +Message: ?string |};

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and the props of './MyBanner.component'.
function MyBannerTypeChecked(props: Props): React$Node {
  return <MyBanner {...props}/>;
};
exports.MyBannerTypeChecked = MyBannerTypeChecked

// Export 'make' early to allow circular import from the '.bs.js' file.
const make: mixed = function (show: $any, Message: $any, children: $any) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, Message: Message}, children); };;
exports.make = make
