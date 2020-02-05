/* TypeScript file generated from Common.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const CommonBS = require('./Common.bs');

// tslint:disable-next-line:interface-over-type-literal
export type t = { readonly lib: string };

export const foo: (_1:void) => t = function (Arg1: any) {
  const result = CommonBS.foo(Arg1);
  return {lib:result[0]}
};
