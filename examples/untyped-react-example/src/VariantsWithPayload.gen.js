/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from VariantsWithPayload.re by genType.
 */
/* eslint-disable */

import * as VariantsWithPayloadBS from './VariantsWithPayload.bs';

export const testWithPayload = VariantsWithPayloadBS.testWithPayload;

export const printVariantWithPayload = VariantsWithPayloadBS.printVariantWithPayload;

export const testManyPayloads = VariantsWithPayloadBS.testManyPayloads;

export const printManyPayloads = VariantsWithPayloadBS.printManyPayloads;

export const testSimpleVariant = function (Arg1) {
  const result = VariantsWithPayloadBS.testSimpleVariant(Arg1);
  return result
};

export const testVariantWithPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value}
      : Arg1.tag==="C"
      ? {TAG: 1, _0:Arg1.value[0], _1:Arg1.value[1]}
      : Arg1.tag==="D"
      ? {TAG: 2, _0:Arg1.value}
      : {TAG: 3, _0:Arg1.value[0], _1:Arg1.value[1], _2:Arg1.value[2]}
    : Arg1);
  return typeof(result) === 'object'
    ? result.TAG===0
      ? {tag:"B", value:result._0}
      : result.TAG===1
      ? {tag:"C", value:result}
      : result.TAG===2
      ? {tag:"D", value:result._0}
      : {tag:"E", value:result}
    : result
};

export const printVariantWithPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.printVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value}
      : Arg1.tag==="C"
      ? {TAG: 1, _0:Arg1.value[0], _1:Arg1.value[1]}
      : Arg1.tag==="D"
      ? {TAG: 2, _0:Arg1.value}
      : {TAG: 3, _0:Arg1.value[0], _1:Arg1.value[1], _2:Arg1.value[2]}
    : Arg1);
  return result
};

export const testVariant1Int = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariant1Int({TAG: 0, _0:Arg1.value});
  return {tag:"R", value:result._0}
};

export const testVariant1Object = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariant1Object({TAG: 0, _0:Arg1});
  return result._0
};
