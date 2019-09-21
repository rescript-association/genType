type t;

type eq = (t, t);

let applyEquations: (~eqs: list(eq), t) => list(eq);

type moduleItemsEmitter;

let createModuleItemsEmitter: unit => moduleItemsEmitter;

let extendExportModules: (~moduleItemsEmitter: moduleItemsEmitter, t) => unit;

let emitAllModuleItems:
  (~emitters: Emitters.t, moduleItemsEmitter) => Emitters.t;

let dot: (string, t) => t;

let fromString: string => t;

let toString: t => string;