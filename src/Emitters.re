type t = {
  requireEmitter: list(string),
  importEmitter: list(string),
  exportEmitter: list(string),
};

let initial = {requireEmitter: [], importEmitter: [], exportEmitter: []};

let string = (~emitter, s) => [s, ...emitter];

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

let toString = (~separator, emitters) =>
  [
    emitters.requireEmitter |> List.rev,
    emitters.importEmitter |> List.rev,
    emitters.exportEmitter |> List.rev,
  ]
  |> List.concat
  |> String.concat(separator);