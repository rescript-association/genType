/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from VariantsWithPayload.re by genType.
 */
/* eslint-disable */

const $$toJS13337556 = {"0": "ARenamed"};

const $$toRE13337556 = {"ARenamed": 0};

const $$toJS346759412 = {"0": "A", "1": "B", "2": "C"};

const $$toRE346759412 = {"A": 0, "B": 1, "C": 2};

const $$toJS435467058 = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20, "803296723": 0.5};

const $$toRE435467058 = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695, "0.5": 803296723};

import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

import * as VariantsWithPayloadBS from './VariantsWithPayload.bs';

export const testWithPayload = function (Arg1) {
  const result = VariantsWithPayloadBS.testWithPayload(typeof(Arg1) === 'object'
    ? [/* c */99, Arg1]
    : $$toRE435467058[Arg1.toString()]);
  return typeof(result) === 'object'
    ? result[1]
    : $$toJS435467058[result]
};

export const printVariantWithPayload = function (Arg1) {
  const result = VariantsWithPayloadBS.printVariantWithPayload(typeof(Arg1) === 'object'
    ? [/* c */99, Arg1]
    : $$toRE435467058[Arg1.toString()]);
  return result
};

export const testManyPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.testManyPayloads(Arg1.tag==="oneRenamed"
    ? [/* one */5544550, Arg1.value]
    : Arg1.tag===2
    ? [/* two */5795212, Arg1.value]
    : [/* three */261117022, Arg1.value]);
  return result[0]===/* one */5544550
    ? {tag:"oneRenamed", value:result[1]}
    : result[0]===/* two */5795212
    ? {tag:2, value:result[1]}
    : {tag:"three", value:result[1]}
};

export const printManyPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.printManyPayloads(Arg1.tag==="oneRenamed"
    ? [/* one */5544550, Arg1.value]
    : Arg1.tag===2
    ? [/* two */5795212, Arg1.value]
    : [/* three */261117022, Arg1.value]);
  return result
};

export const testSimpleVariant = function (Arg1) {
  const result = VariantsWithPayloadBS.testSimpleVariant($$toRE346759412[Arg1]);
  return $$toJS346759412[result]
};

export const testVariantWithPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? CreateBucklescriptBlock.__(0, [Arg1.value])
      : Arg1.tag==="C"
      ? CreateBucklescriptBlock.__(1, Arg1.value)
      : Arg1.tag==="D"
      ? CreateBucklescriptBlock.__(2, [Arg1.value])
      : CreateBucklescriptBlock.__(3, Arg1.value)
    : $$toRE13337556[Arg1]);
  return typeof(result) === 'object'
    ? result.tag===0
      ? {tag:"B", value:result[0]}
      : result.tag===1
      ? {tag:"C", value:result}
      : result.tag===2
      ? {tag:"D", value:result[0]}
      : {tag:"E", value:result}
    : $$toJS13337556[result]
};

export const printVariantWithPayloads = function (Arg1) {
  const result = VariantsWithPayloadBS.printVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? CreateBucklescriptBlock.__(0, [Arg1.value])
      : Arg1.tag==="C"
      ? CreateBucklescriptBlock.__(1, Arg1.value)
      : Arg1.tag==="D"
      ? CreateBucklescriptBlock.__(2, [Arg1.value])
      : CreateBucklescriptBlock.__(3, Arg1.value)
    : $$toRE13337556[Arg1]);
  return result
};

export const testVariant1Int = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariant1Int(CreateBucklescriptBlock.__(0, [Arg1.value]));
  return {tag:"R", value:result[0]}
};

export const testVariant1Object = function (Arg1) {
  const result = VariantsWithPayloadBS.testVariant1Object(CreateBucklescriptBlock.__(0, [Arg1]));
  return result[0]
};
