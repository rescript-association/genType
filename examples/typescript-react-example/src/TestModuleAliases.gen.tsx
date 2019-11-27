/* TypeScript file generated from TestModuleAliases.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const TestModuleAliasesBS = require('./TestModuleAliases.bs');

import {InnerAlias_inner as ModuleAliases2_InnerAlias_inner} from './ModuleAliases2.gen';

import {Outer_Inner_inner as ModuleAliases2_Outer_Inner_inner} from './ModuleAliases2.gen';

import {Outer_outer as ModuleAliases2_Outer_outer} from './ModuleAliases2.gen';

import {record as ModuleAliases2_record} from './ModuleAliases2.gen';

// tslint:disable-next-line:interface-over-type-literal
export type record = ModuleAliases2_record;

// tslint:disable-next-line:interface-over-type-literal
export type record2 = ModuleAliases2_record;

// tslint:disable-next-line:interface-over-type-literal
export type outer = ModuleAliases2_Outer_outer;

// tslint:disable-next-line:interface-over-type-literal
export type outer2 = ModuleAliases2_Outer_outer;

// tslint:disable-next-line:interface-over-type-literal
export type my2 = ModuleAliases2_Outer_Inner_inner;

// tslint:disable-next-line:interface-over-type-literal
export type inner1 = ModuleAliases2_InnerAlias_inner;

// tslint:disable-next-line:interface-over-type-literal
export type inner2 = ModuleAliases2_Outer_Inner_inner;

export const testInner1: (_1:inner1) => inner1 = function (Arg1: any) {
  const result = TestModuleAliasesBS.testInner1([Arg1.inner]);
  return {inner:result[0]}
};

export const testInner1Expanded: (_1:ModuleAliases2_InnerAlias_inner) => ModuleAliases2_InnerAlias_inner = function (Arg1: any) {
  const result = TestModuleAliasesBS.testInner1Expanded([Arg1.inner]);
  return {inner:result[0]}
};

export const testInner2: (_1:inner2) => inner2 = function (Arg1: any) {
  const result = TestModuleAliasesBS.testInner2([Arg1.inner]);
  return {inner:result[0]}
};

export const testInner2Expanded: (_1:ModuleAliases2_Outer_Inner_inner) => ModuleAliases2_Outer_Inner_inner = function (Arg1: any) {
  const result = TestModuleAliasesBS.testInner2Expanded([Arg1.inner]);
  return {inner:result[0]}
};
