/** 
 * @flow strict
 * @generated from ImportHooks.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import {makeRenamed as makeRenamedNotChecked} from './hookExample';

// flowlint-next-line nonstrict-import:off
import {foo as fooNotChecked} from './hookExample';

// In case of type error, check the type of 'makeRenamed' in 'ImportHooks.re' and './hookExample'.
export const makeRenamedTypeChecked: React$ComponentType<{|
  +person: person, 
  +children: React$Node, 
  +renderMe: renderMe<$any>
|}> = makeRenamedNotChecked;

// Export 'makeRenamed' early to allow circular import from the '.bs.js' file.
export const makeRenamed: mixed = makeRenamedTypeChecked;

// In case of type error, check the type of 'foo' in 'ImportHooks.re' and './hookExample'.
export const fooTypeChecked: ({| +person: person |}) => string = fooNotChecked;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: mixed = function (Argperson: $any) {
  const result = fooTypeChecked({person:Argperson});
  return result
};

export type person = {| +name: string, +age: number |};

export type renderMe<a> = React$ComponentType<{| +randomString: string, +poly: a |}>;
