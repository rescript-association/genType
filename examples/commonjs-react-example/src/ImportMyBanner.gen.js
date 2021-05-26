/** 
 * @flow strict
 * @generated from ImportMyBanner.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
const $$MyBanner = require('./MyBanner.component');

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and './MyBanner.component'.
const makeTypeChecked: <a>({| +show: boolean, +Message: ?string |}, a) => ReasonReact_component<ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_actionless> = $$MyBanner.make;;
exports.makeTypeChecked = makeTypeChecked

// Export 'make' early to allow circular import from the '.bs.js' file.
const make: mixed = function <a>(Argshow: $any, ArgMessage: $any, Arg2: $any) {
  const result = makeTypeChecked({show:Argshow, Message:ArgMessage}, Arg2);
  return result
};;
exports.make = make

// flowlint-next-line nonstrict-import:off
import type {actionless as ReasonReact_actionless} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {component as ReasonReact_component} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {noRetainedProps as ReasonReact_noRetainedProps} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {stateless as ReasonReact_stateless} from '../src/shims/ReactShim.shim';
