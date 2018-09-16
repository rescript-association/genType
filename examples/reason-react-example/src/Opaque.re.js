/* @flow strict */

const OpaqueBS = require("./Opaque.bs");

// No need to import locally visible type t. Make sure it is also marked with @genType

export opaque type t = any;
export const fromInt: (number) => t = OpaqueBS.fromInt;