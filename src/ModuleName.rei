type t;

let compare: (t, t) => int;

/** Used to turn strings read from external files into module names. */

let fromStringUnsafe: string => t;

let toString: t => string;
