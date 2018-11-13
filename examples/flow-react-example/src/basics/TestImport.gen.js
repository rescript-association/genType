/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// flowlint-next-line nonstrict-import:off
import {TopLevelClass as innerStuffContentsNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {ValueStartingWithUpperCaseLetter as valueStartingWithUpperCaseLetterNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {default as defaultValueNotChecked} from './exportNestedValues';

// In case of type error, check the type of 'innerStuffContents' in 'TestImport.re' and './exportNestedValues'.
export const innerStuffContentsTypeChecked: {|+x: number|} = innerStuffContentsNotChecked.MiddleLevelElements.stuff.InnerStuff.innerStuffContents;

// Export 'innerStuffContents' early to allow circular import from the '.bs.js' file.
export const innerStuffContents: mixed = innerStuffContentsTypeChecked;

// In case of type error, check the type of 'valueStartingWithUpperCaseLetter' in 'TestImport.re' and './exportNestedValues'.
export const valueStartingWithUpperCaseLetterTypeChecked: string = valueStartingWithUpperCaseLetterNotChecked;

// Export 'valueStartingWithUpperCaseLetter' early to allow circular import from the '.bs.js' file.
export const valueStartingWithUpperCaseLetter: mixed = valueStartingWithUpperCaseLetterTypeChecked;

// In case of type error, check the type of 'defaultValue' in 'TestImport.re' and './exportNestedValues'.
export const defaultValueTypeChecked: number = defaultValueNotChecked;

// Export 'defaultValue' early to allow circular import from the '.bs.js' file.
export const defaultValue: mixed = defaultValueTypeChecked;
