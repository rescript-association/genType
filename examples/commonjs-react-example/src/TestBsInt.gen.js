/** 
 * @flow strict
 * @generated from TestBsInt.res
 * @nolint
 */
/* eslint-disable */

// flowlint-next-line nonstrict-import:off
const $$bsString = require('./bsString');

// In case of type error, check the type of 'packInt' in 'TestBsInt.re' and './bsString'.
const packIntTypeChecked: (0 | 34 | 35, string, string) => string = $$bsString.packInt;;
exports.packIntTypeChecked = packIntTypeChecked

// Export 'packInt' early to allow circular import from the '.bs.js' file.
const packInt: mixed = packIntTypeChecked;;
exports.packInt = packInt
