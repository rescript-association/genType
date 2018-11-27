type nameGen = Hashtbl.t(string, int);

let newNameGen = () => Hashtbl.create(1);
let name = (~nameGen, s) =>
  switch (Hashtbl.find(nameGen, s)) {
  | n =>
    Hashtbl.replace(nameGen, s, n + 1);
    s ++ string_of_int(n + 1);
  | exception Not_found =>
    Hashtbl.replace(nameGen, s, 0);
    s;
  };

let resultName = (~nameGen) => "result" |> name(~nameGen);

let arg = x => "Arg" ++ x;
let argi = i => i |> string_of_int |> arg;
let argiVariant = i => "V" ++ (i |> argi);
let parens = xs => "(" ++ (xs |> String.concat(", ")) ++ ")";
let brackets = x => "{ " ++ x ++ " }";

let comment = x => "/* " ++ x ++ " */";
let quotes = x => "\"" ++ x ++ "\"";
let array = xs => "[" ++ (xs |> String.concat(", ")) ++ "]";
let funCall = (~args, name) =>
  name ++ "(" ++ (args |> String.concat(", ")) ++ ")";
let funDef = (~args, ~mkBody, functionName) => {
  let (params, vals) = List.split(args);
  "function "
  ++ (functionName == "" ? "_" : functionName)
  ++ (params |> parens)
  ++ " "
  ++ (vals |> mkBody |> brackets);
};