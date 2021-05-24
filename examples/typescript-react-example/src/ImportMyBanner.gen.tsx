/* TypeScript file generated from ImportMyBanner.res by genType. */
/* eslint-disable import/first */


import {make as makeNotChecked} from './MyBanner';

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and './MyBanner'.
export const makeTypeChecked: <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => ReasonReact_component<ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_actionless> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function <a>(Argshow: any, Argmessage: any, Arg2: any) {
  const result = makeTypeChecked({show:Argshow, message:Argmessage}, Arg2);
  return result
} as <a>(_1:{ readonly show: boolean; readonly message?: message }, _2:a) => ReasonReact_component<ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_actionless>;

import type {actionless as ReasonReact_actionless} from '../src/shims/ReactShim.shim';

import type {component as ReasonReact_component} from '../src/shims/ReactShim.shim';

import type {noRetainedProps as ReasonReact_noRetainedProps} from '../src/shims/ReactShim.shim';

import type {stateless as ReasonReact_stateless} from '../src/shims/ReactShim.shim';

// tslint:disable-next-line:interface-over-type-literal
export type message = { readonly text: string };
