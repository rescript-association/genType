/* TypeScript file generated from MoreVariants.re by genType. */
/* eslint-disable import/first */


const $$toJS912209123: { [key: string]: any } = {"type_": "type", "b": "b"};

const $$toRE912209123: { [key: string]: any } = {"type": "type_", "b": "b"};

// tslint:disable-next-line:no-var-requires
const MoreVariantsBS = require('./MoreVariants.bs');

// tslint:disable-next-line:interface-over-type-literal
export type withRenaming = "type" | "b";

// tslint:disable-next-line:interface-over-type-literal
export type withoutRenaming = "type_" | "b";

export const testWithRenaming: (x:withRenaming) => withRenaming = function (Arg1: any) {
  const result = MoreVariantsBS.testWithRenaming($$toRE912209123[Arg1]);
  return $$toJS912209123[result]
};

export const testWithoutRenaming: (x:withoutRenaming) => withoutRenaming = MoreVariantsBS.testWithoutRenaming;
