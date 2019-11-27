/** 
 * @flow strict
 * @generated from Tuples.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Curry from 'bs-platform/lib/es6/curry.js';

// $FlowExpectedError: Reason checked type sufficiently
import * as TuplesBS from './Tuples.bs';

export type coord = [number, number, ?number];

export type coord2 = [number, number, ?number];

export type person = {| +Name: string, +age: number |};

export type couple = [person, person];

export const testTuple: ([number, number]) => number = TuplesBS.testTuple;

export const origin: [number, number, ?number] = TuplesBS.origin;

export const computeArea: ([number, number, ?number]) => number = function (Arg1: $any) {
  const result = TuplesBS.computeArea([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaWithIdent: (coord) => number = function (Arg1: $any) {
  const result = TuplesBS.computeAreaWithIdent([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaNoConverters: ([number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(T1, T2) => [T1, T2, ?T3] = function <T1,T2,T3>(Arg1: $any, Arg2: $any) {
  const result = Curry._2(TuplesBS.coord2d, Arg1, Arg2);
  return result
};

export const getFirstName: (couple) => string = function (Arg1: $any) {
  const result = TuplesBS.getFirstName([[Arg1[0].Name, Arg1[0].age], [Arg1[1].Name, Arg1[1].age]]);
  return result
};

export const marry: (person, person) => couple = function (Arg1: $any, Arg2: $any) {
  const result = Curry._2(TuplesBS.marry, [Arg1.Name, Arg1.age], [Arg2.Name, Arg2.age]);
  return [{Name:result[0][0], age:result[0][1]}, {Name:result[1][0], age:result[1][1]}]
};

export const changeSecondAge: (couple) => couple = function (Arg1: $any) {
  const result = TuplesBS.changeSecondAge([[Arg1[0].Name, Arg1[0].age], [Arg1[1].Name, Arg1[1].age]]);
  return [{Name:result[0][0], age:result[0][1]}, {Name:result[1][0], age:result[1][1]}]
};
