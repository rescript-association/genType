/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const TypeExpansionBS = require('./TypeExpansion.bs');

import type {person as Tuples_person} from '../src/basics/Tuples.re';

export type lowerType = {|+person: Tuples_person|};

export type middleType = {|+lowerType: lowerType|};

export type topType = [number, middleType];

export type A_user = {|+name: string|};

export type b = A_user;

export const testConversion: (topType) => topType = function _(Arg1) { const result = TypeExpansionBS.testConversion([Arg1[0], {lowerType:[Arg1[1].lowerType.person]}]); return [result[0], {lowerType:{person:result[1].lowerType[0]}}] };
