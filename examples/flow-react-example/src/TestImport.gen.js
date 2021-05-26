/** 
 * @flow strict
 * @generated from TestImport.res
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// flowlint-next-line nonstrict-import:off
import {TopLevelClass as innerStuffContentsNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {TopLevelClass as innerStuffContentsAsEmptyObjectNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {ValueStartingWithUpperCaseLetter as valueStartingWithUpperCaseLetterNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {default as defaultValueNotChecked} from './exportNestedValues';

// flowlint-next-line nonstrict-import:off
import {TopLevelClass as makeNotChecked} from './interop/MyBanner.component';

// In case of type error, check the type of 'innerStuffContents' in 'TestImport.re' and './exportNestedValues'.
export const innerStuffContentsTypeChecked: {| +x: number |} = innerStuffContentsNotChecked.MiddleLevelElements.stuff.InnerStuff.innerStuffContents;

// Export 'innerStuffContents' early to allow circular import from the '.bs.js' file.
export const innerStuffContents: mixed = innerStuffContentsTypeChecked;

// In case of type error, check the type of 'innerStuffContentsAsEmptyObject' in 'TestImport.re' and './exportNestedValues'.
export const innerStuffContentsAsEmptyObjectTypeChecked: {||} = innerStuffContentsAsEmptyObjectNotChecked.MiddleLevelElements.stuff.InnerStuff.innerStuffContentsEmpty;

// Export 'innerStuffContentsAsEmptyObject' early to allow circular import from the '.bs.js' file.
export const innerStuffContentsAsEmptyObject: mixed = innerStuffContentsAsEmptyObjectTypeChecked;

// In case of type error, check the type of 'valueStartingWithUpperCaseLetter' in 'TestImport.re' and './exportNestedValues'.
export const valueStartingWithUpperCaseLetterTypeChecked: string = valueStartingWithUpperCaseLetterNotChecked;

// Export 'valueStartingWithUpperCaseLetter' early to allow circular import from the '.bs.js' file.
export const valueStartingWithUpperCaseLetter: mixed = valueStartingWithUpperCaseLetterTypeChecked;

// In case of type error, check the type of 'defaultValue' in 'TestImport.re' and './exportNestedValues'.
export const defaultValueTypeChecked: number = defaultValueNotChecked;

// Export 'defaultValue' early to allow circular import from the '.bs.js' file.
export const defaultValue: mixed = defaultValueTypeChecked;

// In case of type error, check the type of 'make' in 'TestImport.re' and './interop/MyBanner.component'.
export const makeTypeChecked: <a>({| +show: boolean, +Message: ?string |}, a) => ReasonReact_component<ReasonReact_stateless,ReasonReact_noRetainedProps,ReasonReact_actionless> = makeNotChecked.MiddleLevelElements.MyBannerInternal;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function <a>(Argshow: $any, ArgMessage: $any, Arg2: $any) {
  const result = makeTypeChecked({show:Argshow, Message:ArgMessage}, Arg2);
  return result
};

// flowlint-next-line nonstrict-import:off
import type {actionless as ReasonReact_actionless} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {component as ReasonReact_component} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {noRetainedProps as ReasonReact_noRetainedProps} from '../src/shims/ReactShim.shim';

// flowlint-next-line nonstrict-import:off
import type {stateless as ReasonReact_stateless} from '../src/shims/ReactShim.shim';

export type message = {| +text: string |};
