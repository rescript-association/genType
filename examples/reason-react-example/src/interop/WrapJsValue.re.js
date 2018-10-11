/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const MyMath = require("./MyMath");

export const round: (number) => number = MyMath.round;

// $FlowExpectedError: Reason checked type sufficiently
const WrapJsValueBS = require("./WrapJsValue.bs");

export const roundedNumber: number = WrapJsValueBS.roundedNumber;
