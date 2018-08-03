/* @flow strict */

const MyBannerRe = require("./MyBannerRe.bs");
const React = require("react");
const ReasonReact = require("reason-react/src/ReasonReact.js");

import type {Component as ReactComponent} from 'React';
import type {actionless as ReasonReactactionless} from '../../src/shims/ReactShim.shim.js';
import type {component as ReasonReactcomponent} from '../../src/shims/ReactShim.shim.js';
import type {noRetainedProps as ReasonReactnoRetainedProps} from '../../src/shims/ReactShim.shim.js';
import type {stateless as ReasonReactstateless} from '../../src/shims/ReactShim.shim.js';
const MyBanner = require("./MyBanner"); // external MyBanner : ReasonReact.reactClass = "./MyBanner"
export type Props = {|show:bool, message:string, children?:any|};
const component = ReasonReact.wrapReasonForJs(
  MyBannerRe.component,
  (function (jsProps: Props) {
     return MyBannerRe.make(jsProps.show, jsProps.message, jsProps.children);
  }));
function checkJsWrapperType(props: Props) {
      return <MyBanner {...props}> </MyBanner>;
    }

exports.component = (component: React$ComponentType<Props>);