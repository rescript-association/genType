/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const Marcel1BS = require('./Marcel1.bs');

export interface IM_t {+name: string, +surname: string};

export type m = IM_t;

export interface IN_t {+name: string, +surname: string};

export type n = IN_t;

export interface IO_t {+name: string, +surname: string};

export type o = IO_t;

export const testConversion: (n) => n = function _(Arg1) { const result = Marcel1BS.testConversion([Arg1.name, Arg1.surname]); return {name:result[0], surname:result[1]} };
