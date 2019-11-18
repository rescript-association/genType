/* TypeScript file generated from Uncurried.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const UncurriedBS = require('./Uncurried.bs');

// tslint:disable-next-line:interface-over-type-literal
export type u0 = () => string;

// tslint:disable-next-line:interface-over-type-literal
export type u1 = (_1:number) => string;

// tslint:disable-next-line:interface-over-type-literal
export type u2 = (_1:number, _2:string) => string;

// tslint:disable-next-line:interface-over-type-literal
export type u3 = (_1:number, _2:string, _3:number) => string;

// tslint:disable-next-line:interface-over-type-literal
export type auth = { readonly login: (_1:void) => string };

// tslint:disable-next-line:interface-over-type-literal
export type authU = { readonly loginU: () => string };

export const uncurried0: () => string = UncurriedBS.uncurried0;

export const uncurried1: (_1:number) => string = UncurriedBS.uncurried1;

export const uncurried2: (_1:number, _2:string) => string = UncurriedBS.uncurried2;

export const uncurried3: (_1:number, _2:string, _3:number) => string = UncurriedBS.uncurried3;

export const curried3: (_1:number, _2:string, _3:number) => string = function (Arg1: any, Arg2: any, Arg3: any) {
  const result = Curry._3(UncurriedBS.curried3, Arg1, Arg2, Arg3);
  return result
};

export const callback: (_1:((_1:void) => number)) => string = UncurriedBS.callback;

export const callback2: (_1:auth) => string = UncurriedBS.callback2;

export const callback2U: (_1:authU) => string = UncurriedBS.callback2U;

export const sumU: (_1:number, _2:number) => void = UncurriedBS.sumU;

export const sumU2: (_1:number) => (_1:number) => void = UncurriedBS.sumU2;

export const sumCurried: (_1:number, _2:number) => void = function (Arg1: any, Arg2: any) {
  const result = Curry._2(UncurriedBS.sumCurried, Arg1, Arg2);
  return result
};

export const sumLblCurried: (_1:string, _2:{ readonly n: number; readonly m: number }) => void = function (Arg1: any, Arg2: any) {
  const result = Curry._3(UncurriedBS.sumLblCurried, Arg1, Arg2.n, Arg2.m);
  return result
};
