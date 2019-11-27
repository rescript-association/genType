/** 
 * @flow strict
 * @generated from BucklescriptAnnotations.re
 * @nolint
 */
/* eslint-disable */

export type someMutableFields = {|
  mutable0: string, 
  +immutable: number, 
  mutable1: string, 
  mutable2: string
|};

export type someMethods = {|
  +send: (string) => void, 
  +on: (string, ((number) => void)) => void, 
  +threeargs: (number, string, number) => string, 
  +twoArgs: (number, string) => number
|};
