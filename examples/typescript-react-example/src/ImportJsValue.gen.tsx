/* TypeScript file generated from ImportJsValue.re by genType. */
/* eslint-disable import/first */


import {round as roundNotChecked} from './MyMath';

import {area as areaNotChecked} from './MyMath';

import {returnMixedArray as returnMixedArrayNotChecked} from './MyMath';

import {useColor as useColorNotChecked} from './MyMath';

import {higherOrder as higherOrderNotChecked} from './MyMath';

import {convertVariant as convertVariantNotChecked} from './MyMath';

import {polymorphic as polymorphicNotChecked} from './MyMath';

import {default as defaultNotChecked} from './MyMath';

const $$toJS580645844: { [key: string]: any } = {"322339018": "tomato", "-999567389": "gray"};

// tslint:disable-next-line:no-var-requires
const CreateBucklescriptBlock = require('bs-platform/lib/es6/block.js');

// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// In case of type error, check the type of 'round' in 'ImportJsValue.re' and './MyMath'.
export const roundTypeChecked: (_1:number) => number = roundNotChecked;

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round: unknown = roundTypeChecked as (_1:number) => number;

// In case of type error, check the type of 'area' in 'ImportJsValue.re' and './MyMath'.
export const areaTypeChecked: (_1:point) => number = areaNotChecked;

// Export 'area' early to allow circular import from the '.bs.js' file.
export const area: unknown = areaTypeChecked as (_1:point) => number;

// In case of type error, check the type of 'returnMixedArray' in 'ImportJsValue.re' and './MyMath'.
export const returnMixedArrayTypeChecked: () => numberOrString[] = returnMixedArrayNotChecked;

// Export 'returnMixedArray' early to allow circular import from the '.bs.js' file.
export const returnMixedArray: unknown = returnMixedArrayTypeChecked as () => numberOrString[];

// In case of type error, check the type of 'useColor' in 'ImportJsValue.re' and './MyMath'.
export const useColorTypeChecked: (_1:color) => number = useColorNotChecked;

// Export 'useColor' early to allow circular import from the '.bs.js' file.
export const useColor: unknown = function (Arg1: any) {
  const result = useColorTypeChecked($$toJS580645844[Arg1]);
  return result
} as (_1:color) => number;

// In case of type error, check the type of 'higherOrder' in 'ImportJsValue.re' and './MyMath'.
export const higherOrderTypeChecked: (_1:((_1:number, _2:number) => number)) => number = higherOrderNotChecked;

// Export 'higherOrder' early to allow circular import from the '.bs.js' file.
export const higherOrder: unknown = function (Arg1: any) {
  const result = higherOrderTypeChecked(function (Arg11: any, Arg2: any) {
      const result1 = Curry._2(Arg1, Arg11, Arg2);
      return result1
    });
  return result
} as (_1:((_1:number, _2:number) => number)) => number;

// In case of type error, check the type of 'convertVariant' in 'ImportJsValue.re' and './MyMath'.
export const convertVariantTypeChecked: (_1:variant) => variant = convertVariantNotChecked;

// Export 'convertVariant' early to allow circular import from the '.bs.js' file.
export const convertVariant: unknown = function (Arg1: any) {
  const result = convertVariantTypeChecked(Arg1.tag===0
    ? {tag:"I", value:Arg1[0]}
    : {tag:"S", value:Arg1[0]});
  return result.tag==="I"
    ? CreateBucklescriptBlock.__(0, [result.value])
    : CreateBucklescriptBlock.__(1, [result.value])
} as (_1:variant) => variant;

// In case of type error, check the type of 'polymorphic' in 'ImportJsValue.re' and './MyMath'.
export const polymorphicTypeChecked: <a>(_1:a) => a = polymorphicNotChecked;

// Export 'polymorphic' early to allow circular import from the '.bs.js' file.
export const polymorphic: unknown = polymorphicTypeChecked as <a>(_1:a) => a;

// In case of type error, check the type of 'default' in 'ImportJsValue.re' and './MyMath'.
export const defaultTypeChecked: number = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as number;

// tslint:disable-next-line:no-var-requires
const ImportJsValueBS = require('./ImportJsValue.bs');

import {AbsoluteValue as $$AbsoluteValue_t} from './MyMath';

import {num as $$myNum} from './MyMath';

import {num as $$num} from './MyMath';

import {numberOrString as $$numberOrString} from './MyMath';

import {polyType as $$polyType} from './MyMath';

import {stringFunction as $$stringFunction} from './MyMath';

// tslint:disable-next-line:interface-over-type-literal
export type point = { readonly x: number; readonly y?: number };

// tslint:disable-next-line:interface-over-type-literal
export type numberOrString = $$numberOrString;

// tslint:disable-next-line:interface-over-type-literal
export type AbsoluteValue_t = $$AbsoluteValue_t;

// tslint:disable-next-line:interface-over-type-literal
export type stringFunction = $$stringFunction;

// tslint:disable-next-line:interface-over-type-literal
export type color = "tomato" | "gray";

// tslint:disable-next-line:interface-over-type-literal
export type variant = 
    { tag: "I"; value: number }
  | { tag: "S"; value: string };

// tslint:disable-next-line:interface-over-type-literal
export type num = $$num;

// tslint:disable-next-line:interface-over-type-literal
export type myNum = $$myNum;

// tslint:disable-next-line:interface-over-type-literal
export type polyType<a> = $$polyType<a>;

export const roundedNumber: number = ImportJsValueBS.roundedNumber;

export const areaValue: number = ImportJsValueBS.areaValue;

export const useGetProp: (x:AbsoluteValue_t) => number = ImportJsValueBS.useGetProp;

export const useGetAbs: (x:AbsoluteValue_t) => number = ImportJsValueBS.useGetAbs;

export const returnedFromHigherOrder: number = ImportJsValueBS.returnedFromHigherOrder;

export default $$default;
