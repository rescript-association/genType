/** 
 * @flow strict
 * @generated from Records.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'rescript/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as RecordsBS from './Records.bs';

import type {weekday as Types_weekday} from './Types.gen';

export type coord = {|
  +x: number, 
  +y: number, 
  +z?: number
|};

export type coord2 = {|
  +a: number, 
  +b: number, 
  +c: ?number
|};

export type bigType = {|
  +x: number, 
  +y: number, 
  +z: ?{|
    +x: number, 
    +y: number, 
    +z?: {|
      +x: number, 
      +y: number, 
      +z: ?number
    |}
  |}
|};

export type testMutable = {| mutableField: number, +immutableField: number |};

export opaque type innerRecord = mixed;

export type outerRecord = {| +innerRecord: innerRecord |};

export type myRecBsAs = {|
  +jsValid0: string, 
  +type: string, 
  +"the-key": string, 
  +"with\"dquote": string, 
  +"with'squote": string, 
  +"1number": string
|};

export const origin: coord = RecordsBS.origin;

export const computeArea: (coord) => number = RecordsBS.computeArea;

export const coord2d: (number, number) => coord = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(RecordsBS.coord2d, Arg1, Arg2);
  return result
};

export const computeArea2: (coord2) => number = RecordsBS.computeArea2;

export const computeArea3: ({|
  +x: number, 
  +y: number, 
  +z: ?number
|}) => number = RecordsBS.computeArea3;

export const computeArea4: ({|
  +x: number, 
  +y: number, 
  +z?: number
|}) => number = RecordsBS.computeArea4;

export const computeNested: ({|
  +x: number, 
  +y: number, 
  +z?: {|
    +x: number, 
    +y: number, 
    +z?: number
  |}
|}) => number = RecordsBS.computeNested;

export const computeNestedNested: ({|
  +x: number, 
  +y: number, 
  +z?: {|
    +x: number, 
    +y: number, 
    +z?: {|
      +x: number, 
      +y: number, 
      +z?: number
    |}
  |}
|}) => number = RecordsBS.computeNestedNested;

export const computeNestedNestedNullable: ({|
  +x: number, 
  +y: number, 
  +z: ?{|
    +x: number, 
    +y: number, 
    +z: ?{|
      +x: number, 
      +y: number, 
      +z: ?number
    |}
  |}
|}) => number = RecordsBS.computeNestedNestedNullable;

export const computeNestedNestedHalfNullable: (bigType) => number = RecordsBS.computeNestedNestedHalfNullable;

export const useTypeImportedInOtherModule: (Types_weekday) => Types_weekday = RecordsBS.useTypeImportedInOtherModule;

export const convertInner: (innerRecord) => innerRecord = RecordsBS.convertInner;

export const convertOuter: (outerRecord) => outerRecord = RecordsBS.convertOuter;

export const testMyRecBsAs: (myRecBsAs) => Array<string> = RecordsBS.testMyRecBsAs;

export const testMyRecBsAs2: (myRecBsAs) => myRecBsAs = RecordsBS.testMyRecBsAs2;
