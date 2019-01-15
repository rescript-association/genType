/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
const Curry = require('bs-platform/lib/js/curry.js');

// $FlowExpectedError: Reason checked type sufficiently
const GreetingBS = require('./Greeting.bs');

// $FlowExpectedError: Reason checked type sufficiently
const ReasonReact = require('reason-react/src/ReasonReact.js');

// flowlint-next-line nonstrict-import:off
import type {ManyComponents_bike as FlowReactExample_ManyComponents_bike} from './FlowReactExample.gen';

// flowlint-next-line nonstrict-import:off
import type {Mouse_t as ReactEvent_Mouse_t} from '../src/shims/ReactEvent.shim';

const onClick: (ReactEvent_Mouse_t) => void = GreetingBS.onClick;;
exports.onClick = onClick

export type Props = {|
  +message: string, 
  +someNumber: number, 
  +extraGreeting?: string, 
  +children?: mixed
|};

const component: React$ComponentType<Props> = ReasonReact.wrapReasonForJs(
  GreetingBS.component,
  (function _(jsProps: Props) {
     return Curry._4(GreetingBS.make, jsProps.message, jsProps.someNumber, jsProps.extraGreeting, jsProps.children);
  }));;
exports.component = component

exports.default = component;

const testBike: (FlowReactExample_ManyComponents_bike) => FlowReactExample_ManyComponents_bike = GreetingBS.testBike;;
exports.testBike = testBike
