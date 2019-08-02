/*
 * this is a custom header you can add to every file!
 */
import MyBanner from './MyBanner';

import * as ReasonReact from 'reason-react/src/ReasonReact.js';

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make = function (show, message, children) { return ReasonReact.wrapJsForReason(MyBanner, {show: show, message: (message == null ? message : {text:message[0]})}, children); };
