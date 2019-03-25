type t = list(string);

let dot = (s, x) => [s, ...x];

let fromString = x => [x];

let toString = x => x |> List.rev |> String.concat("_");