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

// flowlint-next-line nonstrict-import:off
import {default as defaultNotChecked} from './hookExample';

// $FlowExpectedError: Reason checked type sufficiently
import * as React from 'react';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{|
  +person: person, 
  +children: React$Node, 
  +renderMe: ImportHooks_renderMe<string>
|}> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function hookExample(Arg1: $any) {
  const result = React.createElement(makeTypeChecked, {person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe});
  return result
};

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: React.ComponentType<{|
  +person: person, 
  +children: React$Node, 
  +renderMe: ImportHooks_renderMe<string>
|}> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: mixed = function hookExample(Arg1: $any) {
  const result = React.createElement(defaultTypeChecked, {person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe});
  return result
};

// flowlint-next-line nonstrict-import:off
import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

export type person = {| +name: string, +age: number |};

export default $$default;
