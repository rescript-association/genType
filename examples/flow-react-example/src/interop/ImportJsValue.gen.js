/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import {round as roundNotChecked} from './MyMath';

// flowlint-next-line nonstrict-import:off
import {area as areaNotChecked} from './MyMath';

// flowlint-next-line nonstrict-import:off
import {getValueAtIndex as getValueAtIndexNotChecked} from './MyMath';

// flowlint-next-line nonstrict-import:off
import {functionWithRenamedArgument as functionWithRenamedArgumentNotChecked} from './MyMath';

// In case of type error, check the type of 'round' in 'ImportJsValue.re' and './MyMath'.
export const roundTypeChecked: (number) => number = roundNotChecked;

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round: mixed = roundTypeChecked;

// In case of type error, check the type of 'area' in 'ImportJsValue.re' and './MyMath'.
export const areaTypeChecked: (point) => number = areaNotChecked;

// Export 'area' early to allow circular import from the '.bs.js' file.
export const area: mixed = function (Arg1: $any) {
  const result = areaTypeChecked({x:Arg1[0], y:Arg1[1]});
  return result
};

// In case of type error, check the type of 'getValueAtIndex' in 'ImportJsValue.re' and './MyMath'.
export const getValueAtIndexTypeChecked: (myArray<string>, number) => string = getValueAtIndexNotChecked;

// Export 'getValueAtIndex' early to allow circular import from the '.bs.js' file.
export const getValueAtIndex: mixed = getValueAtIndexTypeChecked;

// In case of type error, check the type of 'functionWithRenamedArgument' in 'ImportJsValue.re' and './MyMath'.
export const functionWithRenamedArgumentTypeChecked: (string, {| +ArgRenamed: string |}) => string = functionWithRenamedArgumentNotChecked;

// Export 'functionWithRenamedArgument' early to allow circular import from the '.bs.js' file.
export const functionWithRenamedArgument: mixed = function (Arg1: $any, ArgArgRenamed: $any) {
  const result = functionWithRenamedArgumentTypeChecked(Arg1, {ArgRenamed:ArgArgRenamed});
  return result
};

// $FlowExpectedError: Reason checked type sufficiently
const ImportJsValueBS = require('./ImportJsValue.bs');

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
