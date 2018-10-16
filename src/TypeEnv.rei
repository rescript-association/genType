type t;

let lookup: (~name: string, t) => option(t);

let newModule: (~name: string, t) => t;

let newType: (~name: string, t) => unit;

let pathToRoot: (~path: string, t) => string;

let root: unit => t;

let toString: t => string;