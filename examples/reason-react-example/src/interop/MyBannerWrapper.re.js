/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const React = require('react');

// $FlowExpectedError: Reason checked type sufficiently
const MyBanner = require('./MyBanner');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

export type Props = {|show: boolean, message: ?string|};

// In case of type error, check the type of 'make' in 'MyBannerWrapper.re' and the props of './MyBanner'.
export function MyBannerTypeChecked(props: Props) {
  return <MyBanner {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function _(show, message, children) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, message: message}, children); };
