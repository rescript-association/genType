/* @flow strict */

var MyBannerRe = require("./MyBannerRe.bs");

import type {actionless as ReasonReactactionless} from './ReasonReact.re';
import type {component as ReasonReactcomponent} from './ReasonReact.re';
import type {noRetainedProps as ReasonReactnoRetainedProps} from './ReasonReact.re';
import type {stateless as ReasonReactstateless} from './ReasonReact.re';
const make = MyBannerRe.make;

exports.make = (make: <T1053>({|show:bool, message:string|}, T1053) => ReasonReactcomponent<ReasonReactstateless,ReasonReactnoRetainedProps,ReasonReactactionless>);