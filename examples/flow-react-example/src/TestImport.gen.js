/** 
 * @flow strict
 * @generated from TestImport.re
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
import {TopLevelClass as TopLevelClass} from './interop/MyBanner.component';

// $FlowExpectedError: Reason checked type sufficiently
import * as React from 'react';

// $FlowExpectedError: Reason checked type sufficiently
import * as ReasonReact from 'reason-react/src/ReasonReact.js';

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

export type Props = {| +show: boolean, +Message: ?string |};

// In case of type error, check the type of 'make' in 'TestImport.re' and the props of './interop/MyBanner.component'.
export function MyBannerInternalTypeChecked(props: Props): React$Node {
  return <TopLevelClass.MiddleLevelElements.MyBannerInternal {...props}/>;
}

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: mixed = function (show: $any, Message: $any, children: $any) { return ReasonReact.wrapJsForReason(TopLevelClass.MiddleLevelElements.MyBannerInternal, {show: show, Message: Message}, children); };

export type message = {| +text: string |};
