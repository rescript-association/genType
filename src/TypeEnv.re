open GenTypeCommon;

type t = {
  mutable componentModuleItem: Runtime.moduleItem,
  mutable map: StringMap.t(entry),
  mutable mapModuleTypes: StringMap.t(Typedtree.signature),
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

let newModule = (~name, x) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.newModule %s %s\n", x |> toString, name);
  };
  let moduleItem = Runtime.moduleItemGen() |> Runtime.newModuleItem;
  let newModuleEnv = {
    componentModuleItem: moduleItem,
    map: StringMap.empty,
    mapModuleTypes: StringMap.empty,
    moduleItem,
    name,
    parent: Some(x),
  };
  x.map = x.map |> StringMap.add(name, Module(newModuleEnv));
  newModuleEnv;
};

let addModuleTypeSignature = (~name, ~signature, x) => {
  if (Debug.typeEnv^) {
    logItem("TypeEnv.addModuleTypeSignature %s %s\n", x |> toString, name);
  };
  x.mapModuleTypes = x.mapModuleTypes |> StringMap.add(name, signature);
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

let rec lookupName = (~name, x) => {
  if (Debug.typeEnv^) {
    logItem("Typenv.lookupName %s id:%s\n", x |> toString, name);
  };
  switch (x.mapModuleTypes |> StringMap.find(name)) {
  | signature => Some(signature)
  | exception Not_found =>
    switch (x.parent) {
    | None => None
    | Some(parent) => parent |> lookupName(~name)
    }
  };
};

let rec lookupPath = (~path, x) =>
  switch (path) {
  | Path.Pident(id) =>
    let name = id |> Ident.name;
    if (Debug.typeEnv^) {
      logItem("Typenv.lookupPath %s id:%s\n", x |> toString, name);
    };
    switch (x.map |> StringMap.find(name)) {
    | Module(y) => Some(y)
    | Type(_) => None
    | exception Not_found =>
      switch (x.parent) {
      | None => None
      | Some(parent) => parent |> lookupPath(~path)
      }
    };
  | Pdot(p, s, _) =>
    switch (x |> lookupPath(~path=p)) {
    | None => None
    | Some(y) =>
      if (Debug.typeEnv^) {
        logItem("Typenv.lookupPath %s s:%s\n", y |> toString, s);
      };
      switch (y.map |> StringMap.find(s)) {
      | Module(y) => Some(y)
      | Type(_) => None
      | exception Not_found => None
      };
    }
  | Papply(_) => None
  };

let lookupModuleTypeSignature = (~path, x) =>
  switch (x |> lookupPath(~path), path) {
  | (Some(y), Pident(id)) => y |> lookupName(~name=id |> Ident.name)
  | (Some(y), Pdot(_, s, _)) => y |> lookupName(~name=s)
  | _ => None
  };

let getCurrentModuleName = (~fileName, x) =>
  x.parent == None ? fileName : x.name |> ModuleName.fromStringUnsafe;

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