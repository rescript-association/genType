/* TypeScript file generated from Tuples.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const TuplesBS = require('./Tuples.bs');

// tslint:disable-next-line:interface-over-type-literal
export type coord = [number, number, (null | undefined | number)];

// tslint:disable-next-line:interface-over-type-literal
export type coord2 = [number, number, (null | undefined | number)];

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

// tslint:disable-next-line:interface-over-type-literal
export type couple = [person, person];

export const testTuple: (_1:[number, number]) => number = TuplesBS.testTuple;

export const origin: [number, number, (null | undefined | number)] = TuplesBS.origin;

export const computeArea: (_1:[number, number, (null | undefined | number)]) => number = function (Arg1: any) {
  const result = TuplesBS.computeArea([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaWithIdent: (_1:coord) => number = function (Arg1: any) {
  const result = TuplesBS.computeAreaWithIdent([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaNoConverters: (_1:[number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(_1:T1, _2:T2) => [T1, T2, (null | undefined | T3)] = function <T1,T2,T3>(Arg1: any, Arg2: any) {
  const result = Curry._2(TuplesBS.coord2d, Arg1, Arg2);
  return result
};

export const getFirstName: (_1:couple) => string = TuplesBS.getFirstName;

export const marry: (_1:person, _2:person) => couple = function (Arg1: any, Arg2: any) {
  const result = Curry._2(TuplesBS.marry, Arg1, Arg2);
  return result
};

export const changeSecondAge: (_1:couple) => couple = TuplesBS.changeSecondAge;
