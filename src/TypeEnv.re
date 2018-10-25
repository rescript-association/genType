open GenTypeCommon;

type t = {
  name: string,
  parent: option(t),
  mutable map: StringMap.t(entry),
  mutable mapModuleTypes: StringMap.t(Typedtree.signature),
  mutable moduleItem: Runtime.moduleItem,
}
and entry =
  | Module(t)
  | Type(string);

let root = () => {
  name: "__root__",
  parent: None,
  map: StringMap.empty,
  mapModuleTypes: StringMap.empty,
  moduleItem: Runtime.moduleItemGen() |> Runtime.newModuleItem,
};

let toString = x => x.name;

let newModule = (~name, x) => {
  if (Debug.typeEnv) {
    logItem("TypeEnv.newModule %s %s\n", x |> toString, name);
  };
  let newModuleEnv = {
    name,
    parent: Some(x),
    map: StringMap.empty,
    mapModuleTypes: StringMap.empty,
    moduleItem: Runtime.moduleItemGen() |> Runtime.newModuleItem,
  };
  x.map = x.map |> StringMap.add(name, Module(newModuleEnv));
  newModuleEnv;
};

let addModuleTypeSignature = (~name, ~signature, x) => {
  if (Debug.typeEnv) {
    logItem("TypeEnv.addModuleTypeTranslation %s %s\n", x |> toString, name);
  };
  x.mapModuleTypes = x.mapModuleTypes |> StringMap.add(name, signature);
};

let newType = (~name, x) => {
  if (Debug.typeEnv) {
    logItem("TypeEnv.newType %s %s\n", x |> toString, name);
  };
  x.map = x.map |> StringMap.add(name, Type(name));
};

let rec lookup = (~name, x) =>
  switch (x.map |> StringMap.find(name)) {
  | _ => Some(x)
  | exception Not_found =>
    switch (x.parent) {
    | None => None
    | Some(parent) => parent |> lookup(~name)
    }
  };

let rec lookupModuleTypeSignature = (~name, x) => {
  if (Debug.typeEnv) {
    logItem(
      "TypeEnv.lookupModuleTypeTranslation %s %s\n",
      x |> toString,
      name,
    );
  };
  switch (x.mapModuleTypes |> StringMap.find(name)) {
  | translation => Some(translation)
  | exception Not_found =>
    switch (x.parent) {
    | None => None
    | Some(parent) => parent |> lookupModuleTypeSignature(~name)
    }
  };
};

let getCurrentModuleName = (~fileName, x) =>
  x.parent == None ? fileName : x.name |> ModuleName.fromStringUnsafe;

let updateModuleItem = (~moduleItem, x) => x.moduleItem = moduleItem;

let rec addModulePath = (~typeEnv, name) =>
  switch (typeEnv.parent) {
  | None => name
  | Some(parent) =>
    typeEnv.name ++ "_" ++ name |> addModulePath(~typeEnv=parent)
  };

let getValueAccessPath = (~name, x) => {
  let rec accessPath = x =>
    switch (x.parent) {
    | None => ""
    | Some(parent) =>
      (parent.parent == None ? x.name : parent |> accessPath)
      ++ "["
      ++ (x.moduleItem |> Runtime.emitModuleItem)
      ++ "]"
    };
  let notNested = x.parent == None;
  notNested ? name : x |> accessPath;
};