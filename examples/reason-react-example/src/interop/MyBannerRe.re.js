/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const React = require("react");
// $FlowExpectedError: Reason checked type sufficiently
const MyBanner = require("./MyBanner");
// $FlowExpectedError: Reason checked type sufficiently
const MyBannerReBS = require("./MyBannerRe.bs");
// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require("reason-react/src/ReasonReact.js");

// $FlowExpectedError: Reason checked type sufficiently
export type Props = {|show:boolean, message:string, children?:any|};
export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  MyBannerReBS.component,
  (function _(jsProps: Props) {
     return MyBannerReBS.make(jsProps.show, jsProps.message, jsProps.children);
  }));
export default component;
export function checkJsWrapperType(props: Props) {
      return <MyBanner {...props}/>;
    }