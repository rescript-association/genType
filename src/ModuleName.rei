type t;
let compare: (t, t) => int;
let createBucklescriptBlock: t;

/** Used to turn strings read from external files into module names. */
let fromStringUnsafe: string => t;

let react: t;
let reasonPervasives: t;
let reasonReact: t;
let toString: t => string;