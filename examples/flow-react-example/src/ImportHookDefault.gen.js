/** 
 * @flow strict
 * @generated from ImportHookDefault.re
 * @nolint
 */
/* eslint-disable */

// flowlint-next-line nonstrict-import:off
import {default as makeNotChecked} from './hookExample';

// flowlint-next-line nonstrict-import:off
import {default as defaultNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: React$ComponentType<{|
  +person: person, 
  +children: React$Node, 
  +renderMe: ImportHooks_renderMe<string>
|}> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = makeTypeChecked;

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: React$ComponentType<{|
  +person: person, 
  +children: React$Node, 
  +renderMe: ImportHooks_renderMe<string>
|}> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: mixed = defaultTypeChecked;

import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

export type person = {| +name: string, +age: number |};

export default $$default;
