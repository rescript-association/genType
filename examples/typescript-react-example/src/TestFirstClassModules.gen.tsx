/* TypeScript file generated from TestFirstClassModules.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const TestFirstClassModulesBS = require('./TestFirstClassModules.bs');

import {firstClassModule as FirstClassModulesInterface_firstClassModule} from './FirstClassModulesInterface.gen';

import {firstClassModule as FirstClassModules_firstClassModule} from './FirstClassModules.gen';

import {record as FirstClassModulesInterface_record} from './FirstClassModulesInterface.gen';

// tslint:disable-next-line:interface-over-type-literal
export type firstClassModuleWithTypeEquations<i,o> = { readonly out: (_1:o) => o; readonly Inner: { readonly inn: (_1:i) => i } };

export const convert: (_1:FirstClassModules_firstClassModule) => FirstClassModules_firstClassModule = TestFirstClassModulesBS.convert;

export const convertInterface: (_1:FirstClassModulesInterface_firstClassModule) => FirstClassModulesInterface_firstClassModule = TestFirstClassModulesBS.convertInterface;

export const convertRecord: (_1:FirstClassModulesInterface_record) => FirstClassModulesInterface_record = function (Arg1: any) {
  const result = TestFirstClassModulesBS.convertRecord([Arg1.x, Arg1.y]);
  return {x:result[0], y:result[1]}
};

export const convertFirstClassModuleWithTypeEquations: <T1,T2>(_1:{ readonly out: ((_1:T1) => T1); readonly Inner: { readonly inn: ((_1:T2) => T2) } }) => { readonly out: (_1:T1) => T1; readonly Inner: { readonly inn: (_1:T2) => T2 } } = TestFirstClassModulesBS.convertFirstClassModuleWithTypeEquations;
