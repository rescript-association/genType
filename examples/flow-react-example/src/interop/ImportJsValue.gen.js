/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as ImportJsValueBS from './ImportJsValue.bs';

// flowlint-next-line nonstrict-import:off
import type {myArray} from './MyMath';

export type point = {| +x: number, +y?: number |};

export type { myArray };

export const myArea: (point) => number = function (Arg1: $any) {
  const result = ImportJsValueBS.myArea([Arg1.x, Arg1.y]);
  return result
};

export const roundedNumber: number = ImportJsValueBS.roundedNumber;

export const areaValue: number = ImportJsValueBS.areaValue;
