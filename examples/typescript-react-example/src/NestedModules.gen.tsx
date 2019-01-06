/* TypeScript file generated by genType. */

// tslint:disable-next-line:no-var-requires
const NestedModulesBS = require('./NestedModules.bs');

// tslint:disable-next-line:interface-over-type-literal
export type Universe_nestedType = string[];

// tslint:disable-next-line:interface-over-type-literal
export type Universe_Nested2_nested2Type = Array<string[]>;

// tslint:disable-next-line:interface-over-type-literal
export type Universe_Nested2_Nested3_nested3Type = Array<Array<string[]>>;

// tslint:disable-next-line:interface-over-type-literal
export type Universe_variant = "A" | {tag: "B", value: string};

export const notNested: number = NestedModulesBS.notNested;

export const Universe_theAnswer: number = NestedModulesBS.Universe[0];

export const Universe_Nested2_nested2Value: number = NestedModulesBS.Universe[2][1];

export const Universe_Nested2_Nested3_nested3Value: string = NestedModulesBS.Universe[2][3][4];

export const Universe_Nested2_Nested3_nested3Function: (_1:Universe_Nested2_nested2Type) => Universe_Nested2_nested2Type = NestedModulesBS.Universe[2][3][5];

export const Universe_Nested2_nested2Function: (_1:Universe_Nested2_Nested3_nested3Type) => Universe_Nested2_Nested3_nested3Type = NestedModulesBS.Universe[2][4];

export const Universe_someString: string = NestedModulesBS.Universe[3];
