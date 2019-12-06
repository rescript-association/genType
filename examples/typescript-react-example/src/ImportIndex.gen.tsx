/* TypeScript file generated from ImportIndex.re by genType. */
/* eslint-disable import/first */


import {default as defaultNotChecked} from './';

import * as React from 'react';

const $$toJS892729597: { [key: string]: any } = {"-899608102": "push", "724060212": "replace"};

// In case of type error, check the type of 'default' in 'ImportIndex.re' and './'.
export const defaultTypeChecked: React.ComponentType<{ readonly methodd?: "push" | "replace" }> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = function (Arg1: any) {
  const $props = {methodd:(Arg1.methodd == null ? Arg1.methodd : $$toJS892729597[Arg1.methodd])};
  const result = React.createElement(defaultTypeChecked, $props);
  return result
} as React.ComponentType<{ readonly methodd?: "push" | "replace" }>;

export default $$default;
