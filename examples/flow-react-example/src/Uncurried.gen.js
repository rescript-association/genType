/** 
 * @flow strict
 * @generated from Uncurried.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as UncurriedBS from './Uncurried.bs';

export type u0 = () => string;

export type u1 = (number) => string;

export type u2 = (number, string) => string;

export type u3 = (number, string, number) => string;

export const uncurried0: () => string = UncurriedBS.uncurried0;

export const uncurried1: (number) => string = UncurriedBS.uncurried1;

export const uncurried2: (number, string) => string = UncurriedBS.uncurried2;

export const uncurried3: (number, string, number) => string = UncurriedBS.uncurried3;

export const curried3: (number, string, number) => string = function (Arg1: $any, Arg2: $any, Arg3: $any) {
  const result = Curry._3(UncurriedBS.curried3, Arg1, Arg2, Arg3);
  return result
};
