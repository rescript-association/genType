type t;

type emitters = {
  requireEmitter: t,
  importEmitter: t,
  exportEmitter: t,
};

let concat: list(t) => t;

let initial: t;

let export: (~emitters: emitters, string) => emitters;

let import: (~emitters: emitters, string) => emitters;

let require: (~emitters: emitters, string) => emitters;

let string: (~emitter: t, string) => t;

let toString: (~separator: string, t) => string;