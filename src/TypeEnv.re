open GenTypeCommon;

type t = {
  mutable componentModuleItem: Runtime.moduleItem,
  mutable map: StringMap.t(entry),
  mutable mapModuleTypes: StringMap.t((Typedtree.signature, t)),
  mutable moduleItem: Runtime.moduleItem,
  name: string,
  parent: option(t),
}
and entry =
  | Module(t)
  | Type(string);

let root = () => {
  let moduleItem = Runtime.moduleItemGen() |> Runtime.newModuleItem;
  {
    componentModuleItem: moduleItem,
    map: StringMap.empty,
    mapModuleTypes: StringMap.empty,
    moduleItem,
    name: "__root__",
    parent: None,
  };
};

let toString = x => x.name;

let createTypeEnv = (~name, parent) => {
  let moduleItem = Runtime.moduleItemGen() |> Runtime.newModuleItem;
  {
    componentModuleItem: moduleItem,
    map: StringMap.empty,
    mapModuleTypes: StringMap.empty,
    moduleItem,
    name,
    parent: Some(parent),
  };
};

let newModule = (~name, x) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newModule %s %s\n", x |> toString, name);
  };
  let newTypeEnv = x |> createTypeEnv(~name);
  x.map = x.map |> StringMap.add(name, Module(newTypeEnv));
  newTypeEnv;
};

let newModuleType = (~name, ~signature, x) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newModuleType %s %s\n", x |> toString, name);
  };
  let newTypeEnv = x |> createTypeEnv(~name);
  x.mapModuleTypes =
    x.mapModuleTypes |> StringMap.add(name, (signature, newTypeEnv));
  newTypeEnv;
};

let newType = (~name, x) => {
  if (Debug.typeEnv^) {
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

let rec lookupModuleType = (~path, x) =>
  switch (path) {
  | [moduleTypeName] =>
    if (Debug.typeEnv^) {
      logItem(
        "Typenv.lookupModuleType %s moduleTypeName:%s\n",
        x |> toString,
        moduleTypeName,
      );
    };
    switch (x.mapModuleTypes |> StringMap.find(moduleTypeName)) {
    | (signature, y) => Some((signature, y))
    | exception Not_found =>
      switch (x.parent) {
      | None => None
      | Some(parent) => parent |> lookupModuleType(~path)
      }
    };
  | [moduleName, ...path1] =>
    if (Debug.typeEnv^) {
      logItem(
        "Typenv.lookupModuleType %s moduleName:%s\n",
        x |> toString,
        moduleName,
      );
    };
    switch (x.map |> StringMap.find(moduleName)) {
    | Module(y) => y |> lookupModuleType(~path=path1)
    | Type(_) => None
    | exception Not_found =>
      switch (x.parent) {
      | None => None
      | Some(parent) => parent |> lookupModuleType(~path)
      }
    };
  | [] => None
  };

let rec pathToList = path =>
  switch (path) {
  | Path.Pident(id) => [id |> Ident.name]
  | Path.Pdot(p, s, _) => [s, ...p |> pathToList]
  | Path.Papply(_) => []
  };

let lookupModuleTypeSignature = (~path, x) => {
  if (Debug.typeEnv^) {
    logItem(
      "TypeEnv.lookupModuleTypeSignature %s %s\n",
      x |> toString,
      path |> Path.name,
    );
  };

  x |> lookupModuleType(~path=path |> pathToList |> List.rev);
};

let getNestedModuleName = x =>
  x.parent == None ? None : Some(x.name |> ModuleName.fromStringUnsafe);

let updateModuleItem = (~nameOpt=None, ~moduleItem, x) => {
  switch (nameOpt) {
  | Some("component") => x.componentModuleItem = moduleItem
  | _ => ()
  };
  x.moduleItem = moduleItem;
};

let rec addModulePath = (~typeEnv, name) =>
  switch (typeEnv.parent) {
  | None => name
  | Some(parent) =>
    typeEnv.name ++ "_" ++ name |> addModulePath(~typeEnv=parent)
  };

let getValueAccessPath = (~component=false, ~name, x) => {
  let rec accessPath = x =>
    switch (x.parent) {
    | None => ""
    | Some(parent) =>
      (parent.parent == None ? x.name : parent |> accessPath)
      ++ "["
      ++ (
        (component ? x.componentModuleItem : x.moduleItem)
        |> Runtime.emitModuleItem
      )
      ++ "]"
    };
  let notNested = x.parent == None;
  notNested ? name : x |> accessPath;
};