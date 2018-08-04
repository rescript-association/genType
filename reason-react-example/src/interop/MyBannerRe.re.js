/* @flow strict */

const MyBannerReBS = require("./MyBannerRe.bs");
const React = require("react");
const ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {component as ReasonReactcomponent} from '../../src/shims/ReactShim.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
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

