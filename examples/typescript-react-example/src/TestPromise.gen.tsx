/* TypeScript file generated from TestPromise.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const TestPromiseBS = require('./TestPromise.bs');

// tslint:disable-next-line:interface-over-type-literal
export type promise<a> = Promise<a>;

// tslint:disable-next-line:interface-over-type-literal
export type fromPayload = { readonly x: number; readonly s: string };

// tslint:disable-next-line:interface-over-type-literal
export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = function (Arg1: any) {
  const result = TestPromiseBS.convert(Arg1.then(function _element($promise: any) { return [$promise.x, $promise.s]}));
  return result.then(function _element($promise1: any) { return {result:$promise1[0]}})
};
