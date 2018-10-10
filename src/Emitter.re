type t = list(string);

type emitters = {
  requireEmitter: t,
  importEmitter: t,
  exportEmitter: t,
};

let string = (~emitter, s) => s == "" ? emitter : [s, ...emitter];

let require = (~emitters, s) => {
  ...emitters,
  requireEmitter: s |> string(~emitter=emitters.requireEmitter),
};
let import = (~emitters, s) => {
  ...emitters,
  importEmitter: s |> string(~emitter=emitters.importEmitter),
};

let export = (~emitters, s) => {
  ...emitters,
  exportEmitter: s |> string(~emitter=emitters.exportEmitter),
};

let initial = [];


let concat = emitters => emitters |> List.rev |> List.concat;
let toString = (~separator, emitter) =>
  emitter |> List.rev |> String.concat(separator);