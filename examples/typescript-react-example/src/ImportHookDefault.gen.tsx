/* TypeScript file generated from ImportHookDefault.re by genType. */
/* eslint-disable import/first */


import {default as makeNotChecked} from './hookExample';

import {default as defaultNotChecked} from './hookExample';

import * as React from 'react';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: JSX.Element; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = function hookExample(Arg1: any) {
  const $props = {person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe};
  const result = React.createElement(makeTypeChecked, $props);
  return result
} as React.ComponentType<{
  readonly person: person; 
  readonly children: JSX.Element; 
  readonly renderMe: ImportHooks_renderMe<string>
}>;

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: JSX.Element; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = function hookExample(Arg1: any) {
  const $props = {person:{name:Arg1.person[0], age:Arg1.person[1]}, children:Arg1.children, renderMe:Arg1.renderMe};
  const result = React.createElement(defaultTypeChecked, $props);
  return result
} as React.ComponentType<{
  readonly person: person; 
  readonly children: JSX.Element; 
  readonly renderMe: ImportHooks_renderMe<string>
}>;

import {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

export default $$default;
