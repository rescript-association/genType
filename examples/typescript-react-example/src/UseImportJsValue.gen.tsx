/* TypeScript file generated from UseImportJsValue.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const UseImportJsValueBS = require('./UseImportJsValue.bs');

import {AbsoluteValue_t as ImportJsValue_AbsoluteValue_t} from './ImportJsValue.gen';

import {stringFunction as ImportJsValue_stringFunction} from './ImportJsValue.gen';

export const useGetProp: (x:ImportJsValue_AbsoluteValue_t) => number = UseImportJsValueBS.useGetProp;

export const useTypeImportedInOtherModule: (x:ImportJsValue_stringFunction) => ImportJsValue_stringFunction = UseImportJsValueBS.useTypeImportedInOtherModule;
