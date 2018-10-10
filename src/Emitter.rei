type t;

let initial: t;

let string: (~emitter: t, string) => t;

let concat: list(t) => t;

let toString: (~separator: string, t) => string;