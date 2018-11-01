/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const RecordsBS = require('./Records.bs');

import type {weekday as Types_weekday} from './Types.re';

export interface Icoord {
  +x: number, 
  +y: number, 
  +z?: number
};

export interface Icoord2 {
  +a: number, 
  +b: number, 
  +c: ?number
};

export interface IbigType {
  +x: number, 
  +y: number, 
  +z: ?{
    +x: number, 
    +y: number, 
    +z?: {
      +x: number, 
      +y: number, 
      +z: ?number
    }
  }
};

export interface ItestMutable {mutableField: number, +immutableField: number};

export opaque type IinnerRecord = mixed;

export interface IouterRecord {+innerRecord: IinnerRecord};

export const origin: Icoord = {x:RecordsBS.origin[0], y:RecordsBS.origin[1], z:RecordsBS.origin[2]};

export const computeArea: (Icoord) => number = function _(Arg1) { const result = RecordsBS.computeArea([Arg1.x, Arg1.y, Arg1.z]); return result };

export const coord2d: (number, number) => Icoord = function _(Arg1, Arg2) { const result = RecordsBS.coord2d(Arg1, Arg2); return {x:result[0], y:result[1], z:result[2]} };

export const computeArea2: (Icoord2) => number = function _(Arg1) { const result = RecordsBS.computeArea2([Arg1.a, Arg1.b, Arg1.c]); return result };

export const computeArea3: ({
  +x: number, 
  +y: number, 
  +z: ?number
}) => number = RecordsBS.computeArea3;

export const computeArea4: ({
  +x: number, 
  +y: number, 
  +z?: number
}) => number = RecordsBS.computeArea4;

export const computeNested: ({
  +x: number, 
  +y: number, 
  +z?: {
    +x: number, 
    +y: number, 
    +z?: number
  }
}) => number = RecordsBS.computeNested;

export const computeNestedNested: ({
  +x: number, 
  +y: number, 
  +z?: {
    +x: number, 
    +y: number, 
    +z?: {
      +x: number, 
      +y: number, 
      +z?: number
    }
  }
}) => number = RecordsBS.computeNestedNested;

export const computeNestedNestedNullable: ({
  +x: number, 
  +y: number, 
  +z: ?{
    +x: number, 
    +y: number, 
    +z: ?{
      +x: number, 
      +y: number, 
      +z: ?number
    }
  }
}) => number = RecordsBS.computeNestedNestedNullable;

export const computeNestedNestedHalfNullable: (IbigType) => number = RecordsBS.computeNestedNestedHalfNullable;

export const useTypeImportedInOtherModule: (Types_weekday) => Types_weekday = RecordsBS.useTypeImportedInOtherModule;

export const convertInner: (IinnerRecord) => IinnerRecord = RecordsBS.convertInner;

export const convertOuter: (IouterRecord) => IouterRecord = function _(Arg1) { const result = RecordsBS.convertOuter([Arg1.innerRecord]); return {innerRecord:result[0]} };
