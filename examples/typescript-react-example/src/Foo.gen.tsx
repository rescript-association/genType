/* TypeScript file generated by genType. */
/* eslint-disable import/first */


import {default as makeNotChecked} from './Foo';

// In case of type error, check the type of 'make' in 'Foo.re' and './Foo'.
export const makeTypeChecked: React.FC<{ readonly renderMe: React.FC<{ readonly randomString: string }> }> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as React.FC<{ readonly renderMe: React.FC<{ readonly randomString: string }> }>;
