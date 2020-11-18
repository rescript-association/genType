open SharedTypes;

/* TODO maybe keep track of the "current module path" */
/* maybe add a "current module path" for debugging purposes */
type queryEnv = {
  file,
  exported,
};

let fileEnv = file => {file, exported: file.contents.exported};

let tupleOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (
  pos_lnum - 1,
  pos_cnum - pos_bol,
);
let locationIsBefore = ({Location.loc_start}, pos) =>
  tupleOfLexing(loc_start) <= pos;

let findInScope = (pos, name, stamps) => {
  /* Log.log("Find " ++ name ++ " with " ++ string_of_int(Hashtbl.length(stamps)) ++ " stamps"); */
  Hashtbl.fold(
    (_stamp, declared, result) =>
      if (declared.name.txt == name) {
        /* Log.log("a stamp " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ string_of_int(l) ++ "," ++ string_of_int(c)); */
        if (locationIsBefore(declared.scopeLoc, pos)) {
          switch (result) {
          | None => Some(declared)
          | Some(current) =>
            if (current.name.loc.loc_start.pos_cnum
                < declared.name.loc.loc_start.pos_cnum) {
              Some(declared);
            } else {
              result;
            }
          };
        } else {
          result;
        };
      } else {
        /* Log.log("wrong name " ++ declared.name.txt); */
        result;
      },
    stamps,
    None,
  );
};

let rec joinPaths = (modulePath, path) => {
  switch (modulePath) {
  | Path.Pident(ident) => (ident.stamp, ident.name, path)
  | Papply(fnPath, _argPath) => joinPaths(fnPath, path)
  | Pdot(inner, name, _) => joinPaths(inner, Nested(name, path))
  };
};

let rec makePath = modulePath => {
  switch (modulePath) {
  | Path.Pident(ident) when ident.stamp === 0 => `GlobalMod(ident.name)
  | Pident(ident) => `Stamp(ident.stamp)
  | Papply(fnPath, _argPath) => makePath(fnPath)
  | Pdot(inner, name, _) => `Path(joinPaths(inner, Tip(name)))
  };
};

let makeRelativePath = (basePath, otherPath) => {
  let rec loop = (base, other, tip) =>
    if (Path.same(base, other)) {
      Some(tip);
    } else {
      switch (other) {
      | Pdot(inner, name, _) => loop(basePath, inner, Nested(name, tip))
      | _ => None
      };
    };
  switch (otherPath) {
  | Path.Pdot(inner, name, _) => loop(basePath, inner, Tip(name))
  | _ => None
  };
};

let rec resolvePathInner = (~env, ~path) => {
  switch (path) {
  | Tip(name) => Some(`Local((env, name)))
  | Nested(subName, subPath) =>
    let%opt stamp = Hashtbl.find_opt(env.exported.modules, subName);
    let%opt {item: kind} = Hashtbl.find_opt(env.file.stamps.modules, stamp);
    findInModule(~env, kind, subPath);
  };
}
and findInModule = (~env, kind, path) => {
  switch (kind) {
  | Structure({exported}) =>
    resolvePathInner(~env={...env, exported}, ~path)
  | Ident(modulePath) =>
    let (stamp, moduleName, fullPath) = joinPaths(modulePath, path);
    if (stamp == 0) {
      Some(`Global((moduleName, fullPath)));
    } else {
      let%opt {item: kind} =
        Hashtbl.find_opt(env.file.stamps.modules, stamp);
      findInModule(~env, kind, fullPath);
    };
  };
};

/* let rec findSubModule = (~env, ~getModule) */

let rec resolvePath = (~env, ~path, ~getModule) => {
  let%opt result = resolvePathInner(~env, ~path);
  switch (result) {
  | `Local(env, name) => Some((env, name))
  | `Global(moduleName, fullPath) =>
    let%opt file = getModule(moduleName);
    resolvePath(
      ~env={file, exported: file.contents.exported},
      ~path=fullPath,
      ~getModule,
    );
  };
};

let resolveFromStamps = (~env, ~path, ~getModule, ~pos) => {
  switch (path) {
  | Tip(name) => Some((env, name))
  | Nested(name, inner) =>
    /* Log.log("Finding from stamps " ++ name); */
    let%opt declared = findInScope(pos, name, env.file.stamps.modules);
    /* Log.log("found it"); */
    let%opt res = findInModule(~env, declared.item, inner);
    switch (res) {
    | `Local(env, name) => Some((env, name))
    | `Global(moduleName, fullPath) =>
      let%opt file = getModule(moduleName);
      resolvePath(
        ~env={file, exported: file.contents.exported},
        ~path=fullPath,
        ~getModule,
      );
    };
  };
};

open Infix;

let fromCompilerPath = (~env, path) => {
  switch (makePath(path)) {
  | `Stamp(stamp) => `Stamp(stamp)
  | `Path(0, moduleName, path) => `Global((moduleName, path))
  | `GlobalMod(name) => `GlobalMod(name)
  | `Path(stamp, _moduleName, path) =>
    let res = {
      let%opt {item: kind} =
        Hashtbl.find_opt(env.file.stamps.modules, stamp);
      let%opt_wrap res = findInModule(~env, kind, path);
      switch (res) {
      | `Local(env, name) => `Exported((env, name))
      | `Global(moduleName, fullPath) => `Global((moduleName, fullPath))
      };
    };
    res |? `Not_found;
  };
};

let resolveModuleFromCompilerPath = (~env, ~getModule, path) => {
  switch (fromCompilerPath(~env, path)) {
  | `Global(moduleName, path) =>
    let%opt file = getModule(moduleName);
    let env = fileEnv(file);
    let%opt (env, name) = resolvePath(~env, ~getModule, ~path);
    let%opt stamp = Hashtbl.find_opt(env.exported.modules, name);
    let%opt declared = Hashtbl.find_opt(env.file.stamps.modules, stamp);
    Some((env, Some(declared)));
  | `Stamp(stamp) =>
    let%opt declared = Hashtbl.find_opt(env.file.stamps.modules, stamp);
    Some((env, Some(declared)));
  | `GlobalMod(moduleName) =>
    let%opt file = getModule(moduleName);
    let env = fileEnv(file);
    Some((env, None));
  | `Not_found => None
  | `Exported(env, name) =>
    let%opt stamp = Hashtbl.find_opt(env.exported.modules, name);
    let%opt declared = Hashtbl.find_opt(env.file.stamps.modules, stamp);
    Some((env, Some(declared)));
  };
};

let resolveFromCompilerPath = (~env, ~getModule, path) => {
  switch (fromCompilerPath(~env, path)) {
  | `Global(moduleName, path) =>
    let res = {
      let%opt file = getModule(moduleName);
      let env = {file, exported: file.contents.exported};
      let%opt_wrap (env, name) = resolvePath(~env, ~getModule, ~path);
      `Exported((env, name));
    };
    res |? `Not_found;
  | `Stamp(stamp) => `Stamp(stamp)
  | `GlobalMod(_) => `Not_found
  | `Not_found => `Not_found
  | `Exported(env, name) => `Exported((env, name))
  };
};

let declaredForExportedTip = (~stamps: stamps, ~exported: exported, name, tip) =>
  switch (tip) {
  | Value =>
    Hashtbl.find_opt(exported.values, name)
    |?> (
      stamp =>
        Hashtbl.find_opt(stamps.values, stamp) |?>> (x => {...x, item: ()})
    )
  | Attribute(_)
  | Constructor(_)
  | Type =>
    Hashtbl.find_opt(exported.types, name)
    |?> (
      stamp =>
        Hashtbl.find_opt(stamps.types, stamp) |?>> (x => {...x, item: ()})
    )
  | Module =>
    Hashtbl.find_opt(exported.modules, name)
    |?> (
      stamp =>
        Hashtbl.find_opt(stamps.modules, stamp) |?>> (x => {...x, item: ()})
    )
  };

let declaredForTip = (~stamps, stamp, tip) =>
  switch (tip) {
  | Value =>
    Hashtbl.find_opt(stamps.values, stamp) |?>> (x => {...x, item: ()})
  | Attribute(_)
  | Constructor(_)
  | Type =>
    Hashtbl.find_opt(stamps.types, stamp) |?>> (x => {...x, item: ()})
  | Module =>
    Hashtbl.find_opt(stamps.modules, stamp) |?>> (x => {...x, item: ()})
  };

let getAttribute = (file, stamp, name) => {
  let%opt {item: {kind}} = Hashtbl.find_opt(file.stamps.types, stamp);
  switch (kind) {
  | Record(labels) =>
    let%opt label = labels |> List.find_opt(label => label.aname.txt == name);
    Some(label);
  | _ => None
  };
};

let getConstructor = (file, stamp, name) => {
  let%opt {item: {kind}} = Hashtbl.find_opt(file.stamps.types, stamp);
  switch (kind) {
  | Variant(constructors) =>
    let%opt const =
      constructors |> List.find_opt(const => const.cname.txt == name);
    Some(const);
  | _ => None
  };
};

let exportedForTip = (~env, name, tip) =>
  switch (tip) {
  | Value => Hashtbl.find_opt(env.exported.values, name)
  | Attribute(_)
  | Constructor(_)
  | Type => Hashtbl.find_opt(env.exported.types, name)
  | Module => Hashtbl.find_opt(env.exported.modules, name)
  };

let rec getSourceUri = (~env, ~getModule, path) =>
  switch (path) {
  | File(uri, _moduleName) => uri
  | NotVisible => env.file.uri
  | IncludedModule(path, inner) =>
    Log.log("INCLUDED MODULE");
    switch (resolveModuleFromCompilerPath(~env, ~getModule, path)) {
    | None =>
      Log.log("NOT FOUND");
      getSourceUri(~env, ~getModule, inner);
    | Some((env, _declared)) => env.file.uri
    };
  | ExportedModule(_, inner) => getSourceUri(~env, ~getModule, inner)
  };
