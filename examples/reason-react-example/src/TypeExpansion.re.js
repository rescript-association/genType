/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const TypeExpansionBS = require('./TypeExpansion.bs');

import type {Iperson as ITuples_person} from '../src/basics/Tuples.re';

export type personFromTuples = ITuples_person;

export interface IlowerType {+person: ITuples_person};

export interface ImiddleType {+lowerType: IlowerType};

export type topType = [number, ImiddleType];

export interface IA_user {+name: string};

export type b = IA_user;

export const testConversion: (topType) => topType = function _(Arg1) { const result = TypeExpansionBS.testConversion([Arg1[0], {lowerType:[[Arg1[1].lowerType.person.name, Arg1[1].lowerType.person.age]]}]); return [result[0], {lowerType:{person:{name:result[1].lowerType[0][0], age:result[1].lowerType[0][1]}}}] };
