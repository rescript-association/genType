/** 
 * @flow strict
 * @generated from TypeExpansion.re
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as TypeExpansionBS from './TypeExpansion.bs';

import type {person as Tuples_person} from './Tuples.gen';

export type personFromTuples = Tuples_person;

export type lowerType = {| +person: Tuples_person |};

export type middleType = {| +lowerType: lowerType |};

export type topType = [number, middleType];

export type A_user = {| +name: string |};

export type b = A_user;

export const testConversion: (topType) => topType = function (Arg1: $any) {
  const result = TypeExpansionBS.testConversion([Arg1[0], {lowerType:{person:{name:Arg1[1].lowerType.person.Name, age:Arg1[1].lowerType.person.age}}}]);
  return [result[0], {lowerType:{person:{Name:result[1].lowerType.person.name, age:result[1].lowerType.person.age}}}]
};
