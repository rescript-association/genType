type t = list(string);

let initial = [];

let string = (~emitter, s) => s == "" ? emitter : [s, ...emitter];

let concat = emitters => emitters |> List.rev |> List.concat;
let toString = (~separator, emitter) =>
  emitter |> List.rev |> String.concat(separator);