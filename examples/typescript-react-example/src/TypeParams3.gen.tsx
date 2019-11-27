/* TypeScript file generated from TypeParams3.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const TypeParams3BS = require('./TypeParams3.bs');

import {items2 as TypeParams2_items2} from './TypeParams2.gen';

import {items as TypeParams2_items} from './TypeParams2.gen';

export const test: (_1:TypeParams2_items) => TypeParams2_items = function (Arg1: any) {
  const result = TypeParams3BS.test(Arg1.map(function _element(ArrayItem: any) { return [ArrayItem.id]}));
  return result.map(function _element(ArrayItem1: any) { return {id:ArrayItem1[0]}})
};

export const test2: (_1:TypeParams2_items2) => TypeParams2_items2 = function (Arg1: any) {
  const result = TypeParams3BS.test2(Arg1.map(function _element(ArrayItem: any) { return [ArrayItem.id]}));
  return result.map(function _element(ArrayItem1: any) { return {id:ArrayItem1[0]}})
};
