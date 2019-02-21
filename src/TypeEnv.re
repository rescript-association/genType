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

let toString = typeEnv => typeEnv.name;

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

let newModule = (~name, typeEnv) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newModule %s %s\n", typeEnv |> toString, name);
  };
  let newTypeEnv = typeEnv |> createTypeEnv(~name);
  typeEnv.map = typeEnv.map |> StringMap.add(name, Module(newTypeEnv));
  newTypeEnv;
};

let newModuleType = (~name, ~signature, typeEnv) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newModuleType %s %s\n", typeEnv |> toString, name);
  };
  let newTypeEnv = typeEnv |> createTypeEnv(~name);
  typeEnv.mapModuleTypes =
    typeEnv.mapModuleTypes |> StringMap.add(name, (signature, newTypeEnv));
  newTypeEnv;
};

let newType = (~name, typeEnv) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newType %s %s\n", typeEnv |> toString, name);
  };
  typeEnv.map = typeEnv.map |> StringMap.add(name, Type(name));
};

let getModule = (~name, typeEnv) =>
  switch (typeEnv.map |> StringMap.find(name)) {
  | Module(typeEnv1) => Some(typeEnv1)
  | Type(_) => None
  | exception Not_found => None
  };

let rec lookup = (~name, typeEnv) =>
  switch (typeEnv.map |> StringMap.find(name)) {
  | _ => Some(typeEnv)
  | exception Not_found =>
    switch (typeEnv.parent) {
    | None => None
    | Some(parent) => parent |> lookup(~name)
    }
  };

let rec lookupModuleType = (~path, typeEnv) =>
  switch (path) {
  | [moduleTypeName] =>
    if (Debug.typeEnv^) {
      logItem(
        "Typenv.lookupModuleType %s moduleTypeName:%s\n",
        typeEnv |> toString,
        moduleTypeName,
      );
    };
    switch (typeEnv.mapModuleTypes |> StringMap.find(moduleTypeName)) {
    | x => Some(x)
    | exception Not_found =>
      switch (typeEnv.parent) {
      | None => None
      | Some(parent) => parent |> lookupModuleType(~path)
      }
    };
  | [moduleName, ...path1] =>
    if (Debug.typeEnv^) {
      logItem(
        "Typenv.lookupModuleType %s moduleName:%s\n",
        typeEnv |> toString,
        moduleName,
      );
    };
    switch (typeEnv.map |> StringMap.find(moduleName)) {
    | Module(typeEnv1) => typeEnv1 |> lookupModuleType(~path=path1)
    | Type(_) => None
    | exception Not_found =>
      switch (typeEnv.parent) {
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

let lookupModuleTypeSignature = (~path, typeEnv) => {
  if (Debug.typeEnv^) {
    logItem(
      "TypeEnv.lookupModuleTypeSignature %s %s\n",
      typeEnv |> toString,
      path |> Path.name,
    );
  };

  typeEnv |> lookupModuleType(~path=path |> pathToList |> List.rev);
};

let getNestedModuleName = typeEnv =>
  typeEnv.parent == None ?
    None : Some(typeEnv.name |> ModuleName.fromStringUnsafe);

let updateModuleItem = (~nameOpt=None, ~moduleItem, typeEnv) => {
  switch (nameOpt) {
  | Some("component") => typeEnv.componentModuleItem = moduleItem
  | _ => ()
  };
  typeEnv.moduleItem = moduleItem;
};

let rec addModulePath = (~typeEnv, name) =>
  switch (typeEnv.parent) {
  | None => name
  | Some(parent) =>
    typeEnv.name ++ "_" ++ name |> addModulePath(~typeEnv=parent)
  };

let getValueAccessPath = (~component=false, ~name, typeEnv) => {
  let rec accessPath = typeEnv =>
    switch (typeEnv.parent) {
    | None => ""
    | Some(parent) =>
      (parent.parent == None ? typeEnv.name : parent |> accessPath)
      ++ "["
      ++ (
        (component ? typeEnv.componentModuleItem : typeEnv.moduleItem)
        |> Runtime.emitModuleItem
      )
      ++ "]"
    };
  let notNested = typeEnv.parent == None;
  notNested ? name : typeEnv |> accessPath;
};