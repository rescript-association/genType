/* TypeScript file generated from TestPromise.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const TestPromiseBS = require('./TestPromise.bs');

// tslint:disable-next-line:interface-over-type-literal
export type promise<a> = Promise<a>;

// tslint:disable-next-line:interface-over-type-literal
export type fromPayload = { readonly x: number; readonly s: string };

// tslint:disable-next-line:interface-over-type-literal
export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = TestPromiseBS.convert;

export const barx: (_1:{ readonly x?: Promise<(null | undefined | string)> }, _2:void) => boolean = function (Arg1: any, Arg2: any) {
  const result = Curry._2(TestPromiseBS.barx, (Arg1.x == null ? undefined : Arg1.x.then(function _element($promise: any) { return ($promise == null ? undefined : $promise)})), Arg2);
  return result
};
