let digConstructor = (~env, ~getModule, path) => {
  switch (Query.resolveFromCompilerPath(~env, ~getModule, path)) {
  | `Not_found => None
  | `Stamp(stamp) =>
    let%opt t = Hashtbl.find_opt(env.file.stamps.types, stamp);
    Some((env, t));
  | `Exported(env, name) =>
    let%opt stamp = Hashtbl.find_opt(env.exported.types, name);
    let%opt t = Hashtbl.find_opt(env.file.stamps.types, stamp);
    Some((env, t));
  | _ => None
  };
};

let showModuleTopLevel =
    (
      ~name,
      ~markdown,
      topLevel: list(SharedTypes.declared(SharedTypes.moduleItem)),
    ) => {
  let contents =
    topLevel
    |> List.map(item =>
         switch (item.SharedTypes.item) {
         /*** TODO pretty print module contents */
         | SharedTypes.MType({decl}) =>
           "  " ++ (decl |> Shared.declToString(item.name.txt))
         | Module(_) => "  module " ++ item.name.txt ++ ";"
         | MValue(typ) =>
           "  let "
           ++ item.name.txt
           ++ ": "
           ++ (typ |> Shared.typeToString)  /* TODO indent */
           ++ ";"
         }
       )
    |> String.concat("\n");
  let full = "module " ++ name ++ " = {" ++ "\n" ++ contents ++ "\n}";
  Some(markdown ? "```\n" ++ full ++ "\n```" : full);
};

let showModule =
    (
      ~markdown,
      ~file: SharedTypes.file,
      ~name,
      declared: option(SharedTypes.declared(SharedTypes.moduleKind)),
    ) => {
  switch (declared) {
  | None => showModuleTopLevel(~name, ~markdown, file.contents.topLevel)
  | Some({item: Structure({topLevel})}) =>
    showModuleTopLevel(~name, ~markdown, topLevel)
  | Some({item: Ident(_)}) => Some("Unable to resolve module reference")
  };
};

open Infix;
let newHover =
    (~rootUri, ~file: SharedTypes.file, ~getModule, ~markdown, ~showPath, loc) => {
  switch (loc) {
  | SharedTypes.Explanation(text) => Some(text)
  | TypeDefinition(_name, _tdecl, _stamp) => None
  | LModule(LocalReference(stamp, _tip)) =>
    let%opt md = Hashtbl.find_opt(file.stamps.modules, stamp);
    let%opt (file, declared) =
      References.resolveModuleReference(~file, ~getModule, md);
    let name =
      switch (declared) {
      | Some(d) => d.name.txt
      | None => file.moduleName
      };
    showModule(~name, ~markdown, ~file, declared);
  | LModule(GlobalReference(moduleName, path, tip)) =>
    let%opt file = getModule(moduleName);
    let env = {Query.file, exported: file.contents.exported};
    let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
    let%opt stamp = Query.exportedForTip(~env, name, tip);
    let%opt md = Hashtbl.find_opt(file.stamps.modules, stamp);
    let%opt (file, declared) =
      References.resolveModuleReference(~file, ~getModule, md);
    let name =
      switch (declared) {
      | Some(d) => d.name.txt
      | None => file.moduleName
      };
    showModule(~name, ~markdown, ~file, declared);
  | LModule(_) => None
  | TopLevelModule(name) =>
    let%opt file = getModule(name);
    showModule(~name=file.moduleName, ~markdown, ~file, None);
  | Typed(_, Definition(_, Attribute(_) | Constructor(_))) => None
  | Constant(t) =>
    Some(
      switch (t) {
      | Const_int(_) => "int"
      | Const_char(_) => "char"
      | Const_string(_) => "string"
      | Const_float(_) => "float"
      | Const_int32(_) => "int32"
      | Const_int64(_) => "int64"
      | Const_nativeint(_) => "int"
      },
    )
  | Typed(t, _) =>
    let typeString = t |> Shared.typeToString;
    let extraTypeInfo = {
      let env = {Query.file, exported: file.contents.exported};
      let%opt path = t |> Shared.digConstructor;
      let%opt (_env, {name: {txt}, item: {decl}}) =
        digConstructor(~env, ~getModule, path);
      Some(decl |> Shared.declToString(txt));
      /* TODO type declaration */
      /* None */
      /* Some(typ.toString()) */
    };

    let codeBlock = text => markdown ? "```\n" ++ text ++ "\n```" : text;
    let typeString = codeBlock(typeString);
    let typeString =
      typeString
      ++ (
        switch (extraTypeInfo) {
        | None => ""
        | Some(extra) => "\n\n" ++ codeBlock(extra)
        }
      );

    Some(
      {
        let%opt ({docstring}, {uri}, res) =
          References.definedForLoc(~file, ~getModule, loc);

        let uri =
          Utils.startsWith(uri, rootUri)
            ? "<root>" ++ Utils.sliceToEnd(uri, String.length(rootUri)) : uri;

        let parts =
          switch (res) {
          | `Declared => [Some(typeString), docstring]
          | `Constructor({cname: {txt}, args}) => [
              Some(typeString),
              Some(
                codeBlock(
                  txt
                  ++ "("
                  ++ (
                    args
                    |> List.map(((t, _)) => {
                         let typeString = t |> Shared.typeToString;
                         typeString;
                       })
                    |> String.concat(", ")
                  )
                  ++ ")",
                ),
              ),
              docstring,
            ]
          | `Attribute(_) => [Some(typeString), docstring]
          };

        let parts = showPath ? parts @ [Some(uri)] : parts;

        Some(String.concat("\n\n", parts |> Utils.filterMap(x => x)));
      }
      |? typeString,
    );
  };
};
