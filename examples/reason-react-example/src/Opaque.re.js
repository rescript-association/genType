/* @flow strict */

// $FlowExpectedError: Reason checked type sufficiently
const OpaqueBS = require("./Opaque.bs");

// No need to import locally visible type t. Make sure it is also marked with @genType

// $FlowExpectedError: Reason checked type sufficiently
export opaque type t = any;
export const fromInt: (number) => t = OpaqueBS.fromInt;