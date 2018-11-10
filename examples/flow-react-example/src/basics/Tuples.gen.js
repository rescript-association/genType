/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as TuplesBS from './Tuples.bs';

export type coord = [number, number, ?number];

export type coord2 = [number, number, ?number];

export type person = {|+name: string, +age: number|};

export type couple = [person, person];

export const testTuple: ([number, number]) => number = TuplesBS.testTuple;

export const origin: [number, number, ?number] = TuplesBS.origin;

export const computeArea: ([number, number, ?number]) => number = function _(Arg1) { const result = TuplesBS.computeArea([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]); return result };

export const computeAreaWithIdent: (coord) => number = function _(Arg1) { const result = TuplesBS.computeAreaWithIdent([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]); return result };

export const computeAreaNoConverters: ([number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(T1, T2) => [T1, T2, ?T3] = TuplesBS.coord2d;

export const getFirstName: (couple) => string = function _(Arg1) { const result = TuplesBS.getFirstName([[Arg1[0].name, Arg1[0].age], [Arg1[1].name, Arg1[1].age]]); return result };

export const marry: (person, person) => couple = function _(Arg1, Arg2) { const result = TuplesBS.marry([Arg1.name, Arg1.age], [Arg2.name, Arg2.age]); return [{name:result[0][0], age:result[0][1]}, {name:result[1][0], age:result[1][1]}] };

export const changeSecondAge: (couple) => couple = function _(Arg1) { const result = TuplesBS.changeSecondAge([[Arg1[0].name, Arg1[0].age], [Arg1[1].name, Arg1[1].age]]); return [{name:result[0][0], age:result[0][1]}, {name:result[1][0], age:result[1][1]}] };
