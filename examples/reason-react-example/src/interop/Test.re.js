/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// BEGIN: added by hand
const MyMath = require("./MyMath");

export const round = function _(x: number): number {
  return MyMath.round(x);
};
// END: added by hand

// $FlowExpectedError: Reason checked type sufficiently
const TestBS = require("./Test.bs");

export const roundedNumber: number = TestBS.roundedNumber;
