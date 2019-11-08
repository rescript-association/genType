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
import {useColor as useColorNotChecked} from './MyMath';

// flowlint-next-line nonstrict-import:off
import {higherOrder as higherOrderNotChecked} from './MyMath';

// flowlint-next-line nonstrict-import:off
import {convertVariant as convertVariantNotChecked} from './MyMath';

const $$toJS580645844 = {"322339018": "tomato", "-999567389": "gray"};

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

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

// In case of type error, check the type of 'useColor' in 'ImportJsValue.re' and './MyMath'.
export const useColorTypeChecked: (color) => number = useColorNotChecked;

// Export 'useColor' early to allow circular import from the '.bs.js' file.
export const useColor: mixed = function (Arg1: $any) {
  const result = useColorTypeChecked($$toJS580645844[Arg1]);
  return result
};

// In case of type error, check the type of 'higherOrder' in 'ImportJsValue.re' and './MyMath'.
export const higherOrderTypeChecked: (((number, number) => number)) => number = higherOrderNotChecked;

// Export 'higherOrder' early to allow circular import from the '.bs.js' file.
export const higherOrder: mixed = function (Arg1: $any) {
  const result = higherOrderTypeChecked(function (Arg11: $any, Arg2: $any) {
      const result1 = Curry._2(Arg1, Arg11, Arg2);
      return result1
    });
  return result
};

// In case of type error, check the type of 'convertVariant' in 'ImportJsValue.re' and './MyMath'.
export const convertVariantTypeChecked: (variant) => variant = convertVariantNotChecked;

// Export 'convertVariant' early to allow circular import from the '.bs.js' file.
export const convertVariant: mixed = function (Arg1: $any) {
  const result = convertVariantTypeChecked(Arg1.tag===0
    ? {tag:"I", value:Arg1[0]}
    : {tag:"S", value:Arg1[0]});
  return result.tag==="I"
    ? CreateBucklescriptBlock.__(0, [result.value])
    : CreateBucklescriptBlock.__(1, [result.value])
};

// $FlowExpectedError: Reason checked type sufficiently
const ImportJsValueBS = require('./ImportJsValue.bs');

// flowlint-next-line nonstrict-import:off
import type {AbsoluteValue as AbsoluteValue_t} from './MyMath';

// flowlint-next-line nonstrict-import:off
import type {stringFunction} from './MyMath';

export type point = {| +x: number, +y?: number |};

export type { AbsoluteValue_t };

export type { stringFunction };

export type color = "tomato" | "gray";

export type variant = 
  | {| tag: "I", value: number |}
  | {| tag: "S", value: string |};

export const roundedNumber: number = ImportJsValueBS.roundedNumber;

export const areaValue: number = ImportJsValueBS.areaValue;

export const useGetProp: (AbsoluteValue_t) => number = ImportJsValueBS.useGetProp;

export const useGetAbs: (AbsoluteValue_t) => number = ImportJsValueBS.useGetAbs;

export const returnedFromHigherOrder: number = ImportJsValueBS.returnedFromHigherOrder;
