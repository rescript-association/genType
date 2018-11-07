/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const React = require('react');

const MyBanner = require('./MyBanner.component');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type Props = {|+show: boolean, +Message: ?string|};

// In case of type error, check the type of 'make' in 'MyBannerWrapper.re' and the props of './MyBanner.component'.
export function MyBannerTypeChecked(props: Props) {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function _(show, Message, children) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, Message: Message}, children); };
