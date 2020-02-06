/* TypeScript file generated from ModuleAliases.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const ModuleAliasesBS = require('./ModuleAliases.bs');

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

export const testNested: (x:InnerNestedAlias_t) => InnerNestedAlias_t = ModuleAliasesBS.testNested;

export const testInner: (x:Outer2Alias_OuterInnerAlias_innerT) => Outer2Alias_OuterInnerAlias_innerT = ModuleAliasesBS.testInner;

export const testInner2: (x:Outer2Alias_Inner2_OuterInnerAlias2_innerT) => Outer2Alias_Inner2_OuterInnerAlias2_innerT = ModuleAliasesBS.testInner2;
