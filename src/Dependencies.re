open GenTypeCommon;

type path =
  | Pid(string)
  | Presolved(string)
  | Pdot(path, string);

let rec handleNamespace = (~name, path) =>
  switch (path) {
  | Pid(_)
  | Presolved(_) => path
  | Pdot(Pid(s), moduleName) when s == name => Pid(moduleName)
  | Pdot(path1, s) => Pdot(path1 |> handleNamespace(~name), s)
  };

let rec resolveTypePath1 = (~typeEnv, typePath) =>
  switch (typePath) {
  | Path.Pident(id) =>
    let name = id |> Ident.name;
    switch (typeEnv |> TypeEnv.lookup(~name)) {
    | None => Pid(name)
    | Some(typeEnv1) =>
      let resolvedName = name |> TypeEnv.addModulePath(~typeEnv=typeEnv1);
      if (Debug.typeResolution^) {
        logItem(
          "resolveTypePath name:%s env:%s resolvedName:%s\n",
          name,
          typeEnv1 |> TypeEnv.toString,
          resolvedName,
        );
      };
      Presolved(resolvedName);
    };
  | Pdot(p, s, _pos) => Pdot(p |> resolveTypePath1(~typeEnv), s)
  | Papply(_) => Presolved("__Papply_unsupported_genType__")
  };

let resolveTypePath = (~config, ~typeEnv, typePath) => {
  let path = typePath |> resolveTypePath1(~typeEnv);
  switch (config.namespace) {
  | None => path
  | Some(name) => path |> handleNamespace(~name)
  };
};

let rec typePathToName = typePath =>
  switch (typePath) {
  | Pid(name) => name
  | Presolved(name) => name
  | Pdot(p, s) => typePathToName(p) ++ "_" ++ s
  };