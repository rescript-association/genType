/* TypeScript file generated from EmitModuleIfNoConversion.re by genType. */
/* eslint-disable import/first */


const $$toRE552311971: { [key: string]: any } = {"A": 0};

// tslint:disable-next-line:no-var-requires
const EmitModuleIfNoConversionBS = require('./EmitModuleIfNoConversion.bs');

// tslint:disable-next-line:interface-over-type-literal
export type t = "A" | { readonly name: string };

export const X_foo: (t:t) => void = function (Arg1: any) {
  const result = EmitModuleIfNoConversionBS.X.foo(typeof(Arg1) === 'object'
    ? {TAG: 0, } as any
    : $$toRE552311971[Arg1]);
  return result
};

export const X_x: number = EmitModuleIfNoConversionBS.X.x;

export const Y_x: string = EmitModuleIfNoConversionBS.Y.x;

export const Y: { x: string } = EmitModuleIfNoConversionBS.Y
