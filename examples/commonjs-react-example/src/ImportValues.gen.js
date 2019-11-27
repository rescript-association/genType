/** 
 * @flow strict
 * @generated from ImportValues.re
 * @nolint
 */
/* eslint-disable */

// flowlint-next-line nonstrict-import:off
const $$exportNestedValues = require('./exportNestedValues');

// flowlint-next-line nonstrict-import:off
const $$exportValues = require('./exportValues');

// In case of type error, check the type of 'exportValues' in 'ImportValues.re' and './exportValues'.
const exportValuesTypeChecked: number = $$exportValues;;
exports.exportValuesTypeChecked = exportValuesTypeChecked

// Export 'exportValues' early to allow circular import from the '.bs.js' file.
const exportValues: mixed = exportValuesTypeChecked;;
exports.exportValues = exportValues

// In case of type error, check the type of 'innerStuffContents' in 'ImportValues.re' and './exportNestedValues'.
const innerStuffContentsTypeChecked: {| +x: number |} = $$exportNestedValues.TopLevelClass.MiddleLevelElements.stuff.InnerStuff.innerStuffContents;;
exports.innerStuffContentsTypeChecked = innerStuffContentsTypeChecked

// Export 'innerStuffContents' early to allow circular import from the '.bs.js' file.
const innerStuffContents: mixed = innerStuffContentsTypeChecked;;
exports.innerStuffContents = innerStuffContents

// flowlint-next-line nonstrict-import:off
import type {someType as $$someType} from './exportValues';

export type someType = $$someType;
