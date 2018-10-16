open GenTypeCommon;

type t = {
  name: string,
  parent: option(t),
  mutable map: StringMap.t(entry),
}
and entry =
  | Module(t)
  | Type(string);

let root = () => {name: "__root__", parent: None, map: StringMap.empty};

let toString = x => x.name;

let newModule = (~name, x) => {
  let newModuleEnv = {name, map: StringMap.empty, parent: Some(x)};
  x.map = x.map |> StringMap.add(name, Module(newModuleEnv));
  newModuleEnv;
};

let newType = (~name, x) =>
  x.map = x.map |> StringMap.add(name, Type(name));

let rec lookup = (~name, x) =>
  switch (x.map |> StringMap.find(name)) {
  | _ => Some(x)
  | exception Not_found =>
    switch (x.parent) {
    | None => None
    | Some(parent) => parent |> lookup(~name)
    }
  };

let rec pathToRoot = (~path, x) =>
  switch (x.parent) {
  | None => path
  | Some(parent) => parent |> pathToRoot(~path=x.name ++ "_" ++ path)
  };