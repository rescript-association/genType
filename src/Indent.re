type t = option(string);

let break = (~indent) =>
  switch (indent) {
  | None => ""
  | Some(s) => "\n" ++ s
  };

let more = indent =>
  switch (indent) {
  | None => None
  | Some(s) => Some("  " ++ s)
  };

let heuristic = (~indent, fields) =>
  fields |> List.length > 2 && indent == None ? Some("") : indent;