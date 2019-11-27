/** 
 * @flow strict
 * @generated from FirstClassModules.re
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as FirstClassModulesBS from './FirstClassModules.bs';

export const firstClassModule: {|
  +x: number, 
  +EmptyInnerModule: {|
  |}, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|} = FirstClassModulesBS.firstClassModule;

export const testConvert: ({|
  +x: number, 
  +EmptyInnerModule: {|
  |}, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|}) => {|
  +x: number, 
  +EmptyInnerModule: {|
  |}, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|} = FirstClassModulesBS.testConvert;

export const someFunctorAsFunction: ({|
  +x: number, 
  +EmptyInnerModule: {|
  |}, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|}) => {| +ww: string |} = FirstClassModulesBS.someFunctorAsFunction;
