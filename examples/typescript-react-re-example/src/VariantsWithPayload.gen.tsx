/* TypeScript file generated from VariantsWithPayload.re by genType. */
/* eslint-disable import/first */


const $$toJS13337556: { [key: string]: any } = {"0": "ARenamed"};

const $$toRE13337556: { [key: string]: any } = {"ARenamed": 0};

const $$toJS346759412: { [key: string]: any } = {"0": "A", "1": "B", "2": "C"};

const $$toRE346759412: { [key: string]: any } = {"A": 0, "B": 1, "C": 2};

const $$toJS435467058: { [key: string]: any } = {"97": "a", "98": "bRenamed", "937218926": true, "-574635695": 20, "803296723": 0.5};

const $$toRE435467058: { [key: string]: any } = {"a": 97, "bRenamed": 98, "true": 937218926, "20": -574635695, "0.5": 803296723};

// tslint:disable-next-line:no-var-requires
const VariantsWithPayloadBS = require('./VariantsWithPayload.bs');

// tslint:disable-next-line:interface-over-type-literal
export type payload = { readonly x: number; readonly y?: string };

// tslint:disable-next-line:interface-over-type-literal
export type withPayload = "a" | "bRenamed" | true | 20 | 0.5 | payload;

// tslint:disable-next-line:interface-over-type-literal
export type manyPayloads = 
    { tag: "oneRenamed"; value: number }
  | { tag: 2; value: [string, string] }
  | { tag: "three"; value: payload };

// tslint:disable-next-line:interface-over-type-literal
export type simpleVariant = "A" | "B" | "C";

// tslint:disable-next-line:interface-over-type-literal
export type variantWithPayloads = 
    "ARenamed"
  | { tag: "B"; value: number }
  | { tag: "C"; value: [number, number] }
  | { tag: "D"; value: [number, number] }
  | { tag: "E"; value: [number, string, number] };

// tslint:disable-next-line:interface-over-type-literal
export type variant1Int = { tag: "R"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type variant1Object = payload;

export const testWithPayload: (x:withPayload) => withPayload = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testWithPayload(typeof(Arg1) === 'object'
    ? {HASH: /* c */99, VAL: Arg1}
    : $$toRE435467058[Arg1.toString()]);
  return typeof(result) === 'object'
    ? result.VAL
    : $$toJS435467058[result]
};

export const printVariantWithPayload: (x:withPayload) => void = function (Arg1: any) {
  const result = VariantsWithPayloadBS.printVariantWithPayload(typeof(Arg1) === 'object'
    ? {HASH: /* c */99, VAL: Arg1}
    : $$toRE435467058[Arg1.toString()]);
  return result
};

export const testManyPayloads: (x:manyPayloads) => manyPayloads = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testManyPayloads(Arg1.tag==="oneRenamed"
    ? {HASH: /* one */5544550, VAL: Arg1.value}
    : Arg1.tag===2
    ? {HASH: /* two */5795212, VAL: Arg1.value}
    : {HASH: /* three */261117022, VAL: Arg1.value});
  return result.HASH===/* one */5544550
    ? {tag:"oneRenamed", value:result.VAL}
    : result.HASH===/* two */5795212
    ? {tag:2, value:result.VAL}
    : {tag:"three", value:result.VAL}
};

export const printManyPayloads: (x:manyPayloads) => void = function (Arg1: any) {
  const result = VariantsWithPayloadBS.printManyPayloads(Arg1.tag==="oneRenamed"
    ? {HASH: /* one */5544550, VAL: Arg1.value}
    : Arg1.tag===2
    ? {HASH: /* two */5795212, VAL: Arg1.value}
    : {HASH: /* three */261117022, VAL: Arg1.value});
  return result
};

export const testSimpleVariant: (x:simpleVariant) => simpleVariant = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testSimpleVariant($$toRE346759412[Arg1]);
  return $$toJS346759412[result]
};

export const testVariantWithPayloads: (x:variantWithPayloads) => variantWithPayloads = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value} as any
      : Arg1.tag==="C"
      ? {TAG: 1, _0:Arg1.value[0], _1:Arg1.value[1]} as any
      : Arg1.tag==="D"
      ? {TAG: 2, _0:Arg1.value} as any
      : {TAG: 3, _0:Arg1.value[0], _1:Arg1.value[1], _2:Arg1.value[2]} as any
    : $$toRE13337556[Arg1]);
  return typeof(result) === 'object'
    ? result.TAG===0
      ? {tag:"B", value:result._0}
      : result.TAG===1
      ? {tag:"C", value:result}
      : result.TAG===2
      ? {tag:"D", value:result._0}
      : {tag:"E", value:result}
    : $$toJS13337556[result]
};

export const printVariantWithPayloads: (x:variantWithPayloads) => void = function (Arg1: any) {
  const result = VariantsWithPayloadBS.printVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value} as any
      : Arg1.tag==="C"
      ? {TAG: 1, _0:Arg1.value[0], _1:Arg1.value[1]} as any
      : Arg1.tag==="D"
      ? {TAG: 2, _0:Arg1.value} as any
      : {TAG: 3, _0:Arg1.value[0], _1:Arg1.value[1], _2:Arg1.value[2]} as any
    : $$toRE13337556[Arg1]);
  return result
};

export const testVariant1Int: (x:variant1Int) => variant1Int = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testVariant1Int({TAG: 0, _0:Arg1.value} as any);
  return {tag:"R", value:result._0}
};

export const testVariant1Object: (x:variant1Object) => variant1Object = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testVariant1Object({TAG: 0, _0:Arg1} as any);
  return result._0
};
