/** 
 * this is a custom header you can add to every file!
 * Untyped file generated from ImportMyBanner.re by genType.
 */
/* eslint-disable */

import MyBanner from './MyBanner';

import * as React from 'react';

import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make = function (show, message, children) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, message: message}, children); };
