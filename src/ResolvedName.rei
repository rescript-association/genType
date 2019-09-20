type t;

type eq = (t, t);

let applyEquations: (~eqs: list(eq), t) => list(eq);

let emitAllModuleItems: unit => string;
let extendExportModules: t => unit;

let dot: (string, t) => t;

let fromString: string => t;

let toString: t => string;