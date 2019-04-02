/* TypeScript file generated by genType. */

// tslint:disable-next-line:no-var-requires
const ModuleEquationsBS = require('./ModuleEquations.bs');

// tslint:disable-next-line:interface-over-type-literal
export type Outer_Inner_innerT = { readonly inner: string };

// tslint:disable-next-line:interface-over-type-literal
export type Outer2_Inner2_InnerNested_t = { readonly nested: number };

// tslint:disable-next-line:interface-over-type-literal
export type Outer2_OuterInnerAlias_innerT = Outer_Inner_innerT;

// tslint:disable-next-line:interface-over-type-literal
export type Outer2_Inner2_OuterInnerAlias2_innerT = Outer2_OuterInnerAlias_innerT;

// tslint:disable-next-line:interface-over-type-literal
export type Outer2Alias_OuterInnerAlias_innerT = Outer2_OuterInnerAlias_innerT;

// tslint:disable-next-line:interface-over-type-literal
export type Outer2Alias_Inner2_OuterInnerAlias2_innerT = Outer2_Inner2_OuterInnerAlias2_innerT;

// tslint:disable-next-line:interface-over-type-literal
export type InnerNestedAlias_t = Outer2_Inner2_InnerNested_t;

export const testNested: (_1:InnerNestedAlias_t) => InnerNestedAlias_t = function (Arg1: any) {
  const result = ModuleEquationsBS.testNested([Arg1.nested]);
  return {nested:result[0]}
};

export const testInner: (_1:Outer2Alias_OuterInnerAlias_innerT) => Outer2Alias_OuterInnerAlias_innerT = function (Arg1: any) {
  const result = ModuleEquationsBS.testInner([Arg1.inner]);
  return {inner:result[0]}
};

export const testInner2: (_1:Outer2Alias_Inner2_OuterInnerAlias2_innerT) => Outer2Alias_Inner2_OuterInnerAlias2_innerT = function (Arg1: any) {
  const result = ModuleEquationsBS.testInner2([Arg1.inner]);
  return {inner:result[0]}
};
