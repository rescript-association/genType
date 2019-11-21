/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

const $$toJS13337556 = {"0": "ARenamed"};

const $$toRE13337556 = {"ARenamed": 0};

const $$toJS346759412 = {"0": "A", "1": "B", "2": "C"};

const $$toRE346759412 = {"A": 0, "B": 1, "C": 2};

const $$toJS435467058 = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20, "803296723": 0.5};

const $$toRE435467058 = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695, "0.5": 803296723};

// $FlowExpectedError: Reason checked type sufficiently
import * as CreateBucklescriptBlock from 'bs-platform/lib/es6/block.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as VariantsWithPayloadBS from './VariantsWithPayload.bs';

export type payload = {| +x: number, +y?: string |};

export type withPayload = "a" | "bRenamed" | true | 20 | 0.5 | payload;

export type manyPayloads = 
    {| tag: "oneRenamed", value: number |}
  | {| tag: 2, value: [string, string] |}
  | {| tag: "three", value: payload |};

export type simpleVariant = "A" | "B" | "C";

export type variantWithPayloads = 
    "ARenamed"
  | {| tag: "B", value: number |}
  | {| tag: "C", value: [number, number] |}
  | {| tag: "D", value: [number, number] |}
  | {| tag: "E", value: [number, string, number] |};

export type variant1Int = {| tag: "R", value: number |};

export type variant1Object = payload;

export const testWithPayload: (withPayload) => withPayload = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.testWithPayload(typeof(Arg1) === 'object'
    ? [/* c */99, [Arg1.x, Arg1.y]]
    : $$toRE435467058[Arg1.toString()]);
  return typeof(result) === 'object'
    ? {x:result[1][0], y:result[1][1]}
    : $$toJS435467058[result]
};

export const printVariantWithPayload: (withPayload) => void = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.printVariantWithPayload(typeof(Arg1) === 'object'
    ? [/* c */99, [Arg1.x, Arg1.y]]
    : $$toRE435467058[Arg1.toString()]);
  return result
};

export const testManyPayloads: (manyPayloads) => manyPayloads = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.testManyPayloads(Arg1.tag==="oneRenamed"
    ? [/* one */5544550, Arg1.value]
    : Arg1.tag===2
    ? [/* two */5795212, Arg1.value]
    : [/* three */261117022, [Arg1.value.x, Arg1.value.y]]);
  return result[0]===/* one */5544550
    ? {tag:"oneRenamed", value:result[1]}
    : result[0]===/* two */5795212
    ? {tag:2, value:result[1]}
    : {tag:"three", value:{x:result[1][0], y:result[1][1]}}
};

export const printManyPayloads: (manyPayloads) => void = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.printManyPayloads(Arg1.tag==="oneRenamed"
    ? [/* one */5544550, Arg1.value]
    : Arg1.tag===2
    ? [/* two */5795212, Arg1.value]
    : [/* three */261117022, [Arg1.value.x, Arg1.value.y]]);
  return result
};

export const testSimpleVariant: (simpleVariant) => simpleVariant = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.testSimpleVariant($$toRE346759412[Arg1]);
  return $$toJS346759412[result]
};

export const testVariantWithPayloads: (variantWithPayloads) => variantWithPayloads = function (Arg1: $any) {
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
      ? {tag:"C", value:result.slice()}
      : result.tag===2
      ? {tag:"D", value:result[0]}
      : {tag:"E", value:result.slice()}
    : $$toJS13337556[result]
};

export const printVariantWithPayloads: (variantWithPayloads) => void = function (Arg1: $any) {
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

export const testVariant1Int: (variant1Int) => variant1Int = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.testVariant1Int(CreateBucklescriptBlock.__(0, [Arg1.value]));
  return {tag:"R", value:result[0]}
};

export const testVariant1Object: (variant1Object) => variant1Object = function (Arg1: $any) {
  const result = VariantsWithPayloadBS.testVariant1Object(CreateBucklescriptBlock.__(0, [[Arg1.x, Arg1.y]]));
  return {x:result[0][0], y:result[0][1]}
};
