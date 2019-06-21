/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import {default as makeNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: ({|
  +person: person, 
  +children: React$Node, 
  +renderMe: ImportHooks_renderMe<string>
|}) => React$Node = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function hookExample(Arg1: $any) {
  const result = makeTypeChecked({person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe});
  return result
};

// flowlint-next-line nonstrict-import:off
import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

export type person = {| +name: string, +age: number |};
