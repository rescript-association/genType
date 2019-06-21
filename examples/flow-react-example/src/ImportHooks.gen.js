/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import {make as makeNotChecked} from './hookExample';

// flowlint-next-line nonstrict-import:off
import {foo as fooNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHooks.re' and './hookExample'.
export const makeTypeChecked: <a>({|
  +person: person, 
  +children: React$Node, 
  +renderMe: renderMe<a>
|}) => React$Node = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function hookExample<a>(Arg1: $any) {
  const result = makeTypeChecked({person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe});
  return result
};

// In case of type error, check the type of 'foo' in 'ImportHooks.re' and './hookExample'.
export const fooTypeChecked: ({| +person: person |}) => string = fooNotChecked;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: mixed = function (Argperson: $any) {
  const result = fooTypeChecked({person:{name:Argperson[0], age:Argperson[1]}});
  return result
};

export type person = {| +name: string, +age: number |};

export type renderMe<a> = ({| +randomString: string, +poly: a |}) => React$Node;
