open GenTypeCommon;

type path =
  | Pexternal(string)
  | Pinternal(ResolvedName.t)
  | Pdot(path, string);

let rec handleNamespace = (~name, path) =>
  switch (path) {
  | Pexternal(_)
  | Pinternal(_) => path
  | Pdot(Pexternal(s), moduleName) when s == name => Pexternal(moduleName)
  | Pdot(path1, s) => Pdot(path1 |> handleNamespace(~name), s)
  };

let rec resolvePath1 = (~typeEnv, path) =>
  switch (path) {
  | Path.Pident(id) =>
    let name = id |> Ident.name;
    switch (typeEnv |> TypeEnv.lookup(~name)) {
    | None => Pexternal(name)
    | Some(typeEnv1) =>
      let resolvedName = name |> TypeEnv.addModulePath(~typeEnv=typeEnv1);
      Pinternal(resolvedName);
    };
  | Pdot(p, s, _pos) => Pdot(p |> resolvePath1(~typeEnv), s)
  | Papply(_) =>
    Pinternal("__Papply_unsupported_genType__" |> ResolvedName.fromString)
  };

let rec typePathToName = typePath =>
  switch (typePath) {
  | Pexternal(name) => name
  | Pinternal(resolvedName) => resolvedName |> ResolvedName.toString
  | Pdot(p, s) => typePathToName(p) ++ "_" ++ s
  };

let rec pathIsInternal = path =>
  switch (path) {
  | Pexternal(_) => false
  | Pinternal(_) => true
  | Pdot(p, _) => p |> pathIsInternal
  };

let resolvePath = (~config, ~typeEnv, path) => {
  let typePath = path |> resolvePath1(~typeEnv);
  if (Debug.typeResolution^) {
    logItem(
      "resolveTypePath path:%s typeEnv:%s %s resolved:%s\n",
      path |> Path.name,
      typeEnv |> TypeEnv.toString,
      typePath |> pathIsInternal ? "Internal" : "External",
      typePath |> typePathToName,
    );
  };
  switch (config.namespace) {
  | None => typePath
  | Some(name) => typePath |> handleNamespace(~name)
  };
};

let rec toResolvedName = (path: path) =>
  switch (path) {
  | Pexternal(name) => name |> ResolvedName.fromString
  | Pinternal(resolvedName) => resolvedName
  | Pdot(p, s) => ResolvedName.dot(s, p |> toResolvedName)
  };

let rec getOuterModuleName = path =>
  switch (path) {
  | Pexternal(name) => name |> ModuleName.fromStringUnsafe
  | Pinternal(resolvedName) =>
    resolvedName |> ResolvedName.toString |> ModuleName.fromStringUnsafe
  | Pdot(path1, _) => path1 |> getOuterModuleName
  };

let rec removeOuterModule = path =>
  switch (path) {
  | Pexternal(_)
  | Pinternal(_) => path
  | Pdot(Pexternal(_), s) =>
    Pexternal(s);
  | Pdot(path1, s) => Pdot(path1 |> removeOuterModule, s)
  };

let pathIsShim = (~config, path) =>
  config.modulesMap |> ModuleNameMap.mem(path |> getOuterModuleName);