/* TypeScript file generated from ImportIndex.re by genType. */
/* eslint-disable import/first */


import {default as defaultNotChecked} from './';

import * as React from 'react';

const $$toJS892729597: { [key: string]: any } = {"-899608102": "push", "724060212": "replace"};

// In case of type error, check the type of 'default' in 'ImportIndex.re' and './'.
export const defaultTypeChecked: React.ComponentType<{ readonly method_?: "push" | "replace" }> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = function (Arg1: any) {
  const $props = {method_:(Arg1.method_ == null ? Arg1.method_ : $$toJS892729597[Arg1.method_])};
  const result = React.createElement(defaultTypeChecked, $props);
  return result
} as React.ComponentType<{ readonly method_?: "push" | "replace" }>;

export default $$default;
