type t;

type eq = (t, t);

let applyEquations: (~eqs: list(eq), t) => list(eq);

let dot: (string, t) => t;

let fromPath: Path.t => t;

let fromString: string => t;

let toString: t => string;