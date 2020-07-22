/* TypeScript file generated from VariantsWithPayload.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const VariantsWithPayloadBS = require('./VariantsWithPayload.bs');

// tslint:disable-next-line:interface-over-type-literal
export type payload = { readonly x: number; readonly y?: string };

// tslint:disable-next-line:interface-over-type-literal
export type withPayload = "a" | "bRenamed" | true | 20 | 0.5 | payload;

// tslint:disable-next-line:interface-over-type-literal
export type manyPayloads = 
    { NAME: "oneRenamed"; VAL: number }
  | { NAME: 2; VAL: [string, string] }
  | { NAME: "three"; VAL: payload };

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

export const testWithPayload: (x:withPayload) => withPayload = VariantsWithPayloadBS.testWithPayload;

export const printVariantWithPayload: (x:withPayload) => void = VariantsWithPayloadBS.printVariantWithPayload;

export const testManyPayloads: (x:manyPayloads) => manyPayloads = VariantsWithPayloadBS.testManyPayloads;

export const printManyPayloads: (x:manyPayloads) => void = VariantsWithPayloadBS.printManyPayloads;

export const testSimpleVariant: (x:simpleVariant) => simpleVariant = function (Arg1: any) {
  const result = VariantsWithPayloadBS.testSimpleVariant(Arg1);
  return result
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

export const printVariantWithPayloads: (x:variantWithPayloads) => void = function (Arg1: any) {
  const result = VariantsWithPayloadBS.printVariantWithPayloads(typeof(Arg1) === 'object'
    ? Arg1.tag==="B"
      ? {TAG: 0, _0:Arg1.value} as any
      : Arg1.tag==="C"
      ? {TAG: 1, _0:Arg1.value[0], _1:Arg1.value[1]} as any
      : Arg1.tag==="D"
      ? {TAG: 2, _0:Arg1.value} as any
      : {TAG: 3, _0:Arg1.value[0], _1:Arg1.value[1], _2:Arg1.value[2]} as any
    : Arg1);
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
