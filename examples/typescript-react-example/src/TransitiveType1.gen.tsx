/* TypeScript file generated from TransitiveType1.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const TransitiveType1BS = require('./TransitiveType1.bs');

import {t2Alias as TransitiveType2_t2Alias} from './TransitiveType2.gen';

import {t2 as TransitiveType2_t2} from './TransitiveType2.gen';

export const convert: (x:TransitiveType2_t2) => TransitiveType2_t2 = function (Arg1: any) {
  const result = TransitiveType1BS.convert((Arg1 == null ? undefined : Arg1));
  return result
};

export const convertAlias: (x:TransitiveType2_t2Alias) => TransitiveType2_t2Alias = function (Arg1: any) {
  const result = TransitiveType1BS.convertAlias((Arg1 == null ? undefined : Arg1));
  return result
};
