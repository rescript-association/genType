/* TypeScript file generated from Unboxed.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const CreateBucklescriptBlock = require('bs-platform/lib/es6/block.js');

// tslint:disable-next-line:no-var-requires
const UnboxedBS = require('./Unboxed.bs');

// tslint:disable-next-line:interface-over-type-literal
export type v1 = { tag: "A"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type v2 = { tag: "A"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type r1 = { readonly x: number };

// tslint:disable-next-line:interface-over-type-literal
export type r2 = { readonly g: string };

export const testV1: (_1:v1) => v1 = function (Arg1: any) {
  const result = UnboxedBS.testV1(CreateBucklescriptBlock.__(0, [Arg1.value]));
  return {tag:"A", value:result[0]}
};

export const r2Test: (_1:r2) => r2 = function (Arg1: any) {
  const result = UnboxedBS.r2Test(CreateBucklescriptBlock.__(0, Arg1));
  return result
};
