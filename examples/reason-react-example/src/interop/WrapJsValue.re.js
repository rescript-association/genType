/** 
 * @flow strict
 * @generated 
 * @nolint
 */

const MyMath = require('./MyMath');

// In case of type error, check the type of 'round' in 'WrapJsValue.re' and './MyMath'.
export const roundTypeChecked: (number) => number = MyMath.round;

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round: (number) => number = roundTypeChecked;

// In case of type error, check the type of 'area' in 'WrapJsValue.re' and './MyMath'.
export const areaTypeChecked: (point) => number = MyMath.area;

// Export 'area' early to allow circular import from the '.bs.js' file.
export const area: (point) => number = function _(Arg1) { const result = areaTypeChecked({x:Arg1[0], y:Arg1[1]}); return result };

// In case of type error, check the type of 'getValueAtIndex' in 'WrapJsValue.re' and './MyMath'.
export const getValueAtIndexTypeChecked: (myArray<string>, number) => string = MyMath.getValueAtIndex;

// Export 'getValueAtIndex' early to allow circular import from the '.bs.js' file.
export const getValueAtIndex: (myArray<string>, number) => string = getValueAtIndexTypeChecked;

// $FlowExpectedError: Reason checked type sufficiently
const WrapJsValueBS = require('./WrapJsValue.bs');

import type {myArray} from './MyMath';

export type point = {|+x: number, +y?: number|};

export type { myArray };

export const roundedNumber: number = WrapJsValueBS.roundedNumber;

export const areaValue: number = WrapJsValueBS.areaValue;
