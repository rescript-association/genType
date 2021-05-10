/* TypeScript file generated from A.res by genType. */
/* eslint-disable import/first */


import {someTrue as someTrueNotChecked} from './MyMath';

// In case of type error, check the type of 'someTrue' in 'A.re' and './MyMath'.
export const someTrueTypeChecked: (_1:true_, _2:number) => string = someTrueNotChecked;

// Export 'someTrue' early to allow circular import from the '.bs.js' file.
export const someTrue: unknown = someTrueTypeChecked as (_1:true_, _2:number) => string;

import {true_ as $$true_} from './MyMath';

// tslint:disable-next-line:interface-over-type-literal
export type true_ = $$true_;
