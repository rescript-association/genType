/* TypeScript file generated from EmitModuleIfNoConversion.res by genType. */
/* eslint-disable import/first */


const $$toRE552311971: { [key: string]: any } = {"A": 0};

import * as EmitModuleIfNoConversionBS from './EmitModuleIfNoConversion.bs';

// tslint:disable-next-line:interface-over-type-literal
export type t = "A" | { readonly name: string };

export const X_foo: (t:t) => void = function (Arg1: any) {
  const result = EmitModuleIfNoConversionBS.X.foo(typeof(Arg1) === 'object'
    ? Object.assign({TAG: 0}, Arg1)
    : $$toRE552311971[Arg1]);
  return result
};

export const X_x: number = EmitModuleIfNoConversionBS.X.x;

export const Y_x: string = EmitModuleIfNoConversionBS.Y.x;

export const Y: { x: string } = EmitModuleIfNoConversionBS.Y
