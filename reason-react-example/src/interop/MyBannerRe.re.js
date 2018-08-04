/* @flow strict */

const MyBannerReBS = require("./MyBannerRe.bs");
const React = require("react");
const ReasonReact = require("reason-react/src/ReasonReact.js");

const MyBanner = require("./MyBanner"); // external MyBanner : ReasonReact.reactClass = "./MyBanner"
export type Props = {|show:bool, message:string, children?:any|};
export const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  MyBannerReBS.component,
  (function _(jsProps: Props) {
     return MyBannerReBS.make(jsProps.show, jsProps.message, jsProps.children);
  }));
function checkJsWrapperType(props: Props) {
      return <MyBanner {...props}> </MyBanner>;
    }

