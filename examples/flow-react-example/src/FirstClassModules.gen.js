/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as FirstClassModulesBS from './FirstClassModules.bs';

export const firstClassModule: {|
  +x: number, 
  +EmptyInnerModule: {
  }, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|} = {x:FirstClassModulesBS.firstClassModule[0], EmptyInnerModule:{}, InnerModule2:{k:FirstClassModulesBS.firstClassModule[2][0]}, Z:FirstClassModulesBS.firstClassModule[3], y:FirstClassModulesBS.firstClassModule[4]};

export const testConvert: ({|
  +x: number, 
  +EmptyInnerModule: {
  }, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|}) => {|
  +x: number, 
  +EmptyInnerModule: {
  }, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|} = function _(Arg1) {
  const result = FirstClassModulesBS.testConvert([Arg1.x, [], [Arg1.InnerModule2.k], Arg1.Z, Arg1.y]);
  return {x:result[0], EmptyInnerModule:{}, InnerModule2:{k:result[2][0]}, Z:result[3], y:result[4]}
};

export const someFunctorAsFunction: ({|
  +x: number, 
  +EmptyInnerModule: {
  }, 
  +InnerModule2: {|
    +k: number
  |}, 
  +Z: mixed, 
  +y: string
|}) => {| +ww: string |} = function _(Arg1) {
  const result = FirstClassModulesBS.someFunctorAsFunction([Arg1.x, [], [Arg1.InnerModule2.k], Arg1.Z, Arg1.y]);
  return {ww:result[0]}
};
