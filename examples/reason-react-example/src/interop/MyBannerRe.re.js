/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const React = require("react");

// $FlowExpectedError: Reason checked type sufficiently
const MyBanner = require("./MyBanner");

export type Props = {|show: boolean, message: ?string|};

export function checkJsWrapperType(props: Props) {
      return <MyBanner {...props}/>;
    }
