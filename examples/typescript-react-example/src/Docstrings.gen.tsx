/* TypeScript file generated from Docstrings.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const DocstringsBS = require('./Docstrings.bs');

/**  hello  */
export const flat: number = DocstringsBS.flat;

/** 
  * Returns the average of two numbers.
  *
  * @param message - A message to be signed
  * @param key - The keypair with which to sign the message
  * @returns A signed message
  */
export const multiline: (message:string, key:number) => string = DocstringsBS.multiline;

export const one: (a:number) => number = DocstringsBS.one;

export const two: (a:number, b:number) => number = function (Arg1: any, Arg2: any) {
  const result = Curry._2(DocstringsBS.two, Arg1, Arg2);
  return result
};

export const tree: (a:number, b:number, c:number) => number = function (Arg1: any, Arg2: any, Arg3: any) {
  const result = Curry._3(DocstringsBS.tree, Arg1, Arg2, Arg3);
  return result
};

export const oneU: (a:number) => number = DocstringsBS.oneU;

export const twoU: (a:number, b:number) => number = DocstringsBS.twoU;

export const treeU: (a:number, b:number, c:number) => number = DocstringsBS.treeU;

export const useParam: (param:number) => number = DocstringsBS.useParam;

export const useParamU: (param:number) => number = DocstringsBS.useParamU;

export const unnamed1: (param:number) => number = DocstringsBS.unnamed1;

export const unnamed1U: (param:number) => number = DocstringsBS.unnamed1U;

export const unnamed2: (param_0:number, param_1:number) => number = function (Arg1: any, Arg2: any) {
  const result = Curry._2(DocstringsBS.unnamed2, Arg1, Arg2);
  return result
};

export const unnamed2U: (param_0:number, param_1:number) => number = DocstringsBS.unnamed2U;

export const grouped: (_1:{ readonly x: number; readonly y: number }, a:number, b:number, c:number, _5:{ readonly z: number }) => number = function (Arg1: any, Arg2: any, Arg3: any, Arg4: any, Arg5: any) {
  const result = Curry._6(DocstringsBS.grouped, Arg1.x, Arg1.y, Arg2, Arg3, Arg4, Arg5.z);
  return result
};
