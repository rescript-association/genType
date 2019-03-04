/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */
// $FlowExpectedError: Reason checked type sufficiently
type $any = any;

// $FlowExpectedError: Reason checked type sufficiently
import * as Marcel1BS from './Marcel1.bs';

export type M_t = {| +name: string, +surname: string |};

export type m = M_t;

export type N_t = {| +name: string, +surname: string |};

export type n = N_t;

export type O_t = {| +name: string, +surname: string |};

export type o = O_t;

export const testConversion: (n) => n = function _(Arg1: $any) {
  const result = Marcel1BS.testConversion([Arg1.name, Arg1.surname]);
  return {name:result[0], surname:result[1]}
};
