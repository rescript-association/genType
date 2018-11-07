import * as MyMath from "./MyMath";

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round =  MyMath.round;

import * as WrapJsValueBS from "./WrapJsValue.bs";
//const WrapJsValueBS = require('./WrapJsValue.bs');

export const roundedNumber: number = WrapJsValueBS.lazy().roundedNumber;
