/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from ImportMyBanner.res by genType.
 */
/* eslint-disable */

import {make as makeNotChecked} from './MyBanner';

// In case of type error, check the type of 'make' in 'ImportMyBanner.re' and './MyBanner'.
export const makeTypeChecked = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make = function (Argshow, Argmessage, Arg2) {
  const result = makeTypeChecked({show:Argshow, message:Argmessage}, Arg2);
  return result
};
