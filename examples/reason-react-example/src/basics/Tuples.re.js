/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const TuplesBS = require('./Tuples.bs');

export const testTuple: ([number, number]) => number = TuplesBS.testTuple;

export type coord = [number, number, ?number];

export const origin: [number, number, ?number] = TuplesBS.origin;

export const computeArea: ([number, number, ?number]) => number = function _(Arg1) { const result = TuplesBS.computeArea([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]); return result };

export const computeAreaWithIdent: (coord) => number = function _(Arg1) { const result = TuplesBS.computeAreaWithIdent([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]); return result };

export const computeAreaNoConverters: ([number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(T1, T2) => [T1, T2, ?T3] = TuplesBS.coord2d;

export type coord2 = [number, number, ?number];

export type person = {|+name: string, +age: number|};

export opaque type couple = mixed;
