/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const MyMath = require("./MyMath");

export const roundTypeChecked: (number) => number = MyMath.round;

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round: (number) => number = roundTypeChecked;

export const areaTypeChecked: (point) => number = MyMath.area;

// Export 'area' early to allow circular import from the '.bs.js' file.
export const area: (point) => number = function _(Arg1) { const result = areaTypeChecked({x:Arg1[0], y:Arg1[1]}); return result };

// $FlowExpectedError: Reason checked type sufficiently
const WrapJsValueBS = require("./WrapJsValue.bs");

export type point = {|x: number, y?: number|};

export const roundedNumber: number = WrapJsValueBS.roundedNumber;

export const areaValue: number = WrapJsValueBS.areaValue;
