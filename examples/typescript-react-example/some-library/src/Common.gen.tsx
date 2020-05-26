/* TypeScript file generated from Common.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const CommonBS = require('./Common.bs');

// tslint:disable-next-line:interface-over-type-literal
export type t = { readonly lib: string };

export const foo: () => t = function () {
  const result = CommonBS.foo();
  return {lib:result[0]}
};
