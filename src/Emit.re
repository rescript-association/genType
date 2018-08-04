let arg = x => "Arg" ++ x;
let argi = i => i |> string_of_int |> arg;
let parens = xs => "(" ++ (xs |> String.concat(", ")) ++ ")";
let brackets = x => "{ " ++ x ++ " }";
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
