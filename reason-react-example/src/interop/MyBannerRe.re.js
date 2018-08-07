/* @flow strict */

const MyBanner = require("./MyBanner");
const MyBannerReBS = require("./MyBannerRe.bs");
const React = require("react");
const ReasonReact = require("reason-react/src/ReasonReact.js");

export type Props = {|show:boolean, message:string, children?:any|};
export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  MyBannerReBS.component,
  (function _(jsProps: Props) {
     return MyBannerReBS.make(jsProps.show, jsProps.message, jsProps.children);
  }));
function checkJsWrapperType(props: Props) {
      return <MyBanner {...props}/>;
    }

