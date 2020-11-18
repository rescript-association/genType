open Infix;

let ifDebug = (debug, name, fn, v) => {
  if (debug) {
    Log.log(name ++ ": " ++ fn(v));
  };
  v;
};

/**
 * Returns a list of paths, relative to the provided `base`
 *
 */
let getSourceDirectories = (~includeDev, base, config) => {
  let rec handleItem = (current, item) => {
    switch (item) {
    | Json.Array(contents) =>
      List.map(handleItem(current), contents) |> List.concat
    | Json.String(text) => [current /+ text]
    | Json.Object(_) =>
      let dir =
        Json.get("dir", item) |?> Json.string |? "Must specify directory";
      let typ =
        includeDev
          ? "lib" : item |> Json.get("type") |?> Json.string |? "lib";
      if (typ == "dev") {
        [];
      } else {
        switch (item |> Json.get("subdirs")) {
        | None
        | Some(Json.False) => [current /+ dir]
        | Some(Json.True) =>
          Files.collectDirs(base /+ current /+ dir)
          /* |> ifDebug(true, "Subdirs", String.concat(" - ")) */
          |> List.filter(name => name != Filename.current_dir_name)
          |> List.map(Files.relpath(base))
        | Some(item) => [
            current /+ dir,
            ...handleItem(current /+ dir, item),
          ]
        };
      };
    | _ => failwith("Invalid subdirs entry")
    };
  };
  config |> Json.get("sources") |?>> handleItem("") |? [];
};

let isCompiledFile = name =>
  Filename.check_suffix(name, ".cmt") || Filename.check_suffix(name, ".cmti");

let isSourceFile = name =>
  Filename.check_suffix(name, ".re")
  || Filename.check_suffix(name, ".rei")
  || Filename.check_suffix(name, ".res")
  || Filename.check_suffix(name, ".resi")
  || Filename.check_suffix(name, ".rel")
  || Filename.check_suffix(name, ".ml")
  || Filename.check_suffix(name, ".mli");

let compiledNameSpace = name =>
  Str.split(Str.regexp_string("-"), name)
  |> List.map(String.capitalize_ascii)
  |> String.concat("")
  /* Remove underscores??? Whyyy bucklescript, whyyyy */
  |> Str.split(Str.regexp_string("_"))
  |> String.concat("");

let compiledBaseName = (~namespace, name) =>
  Filename.chop_extension(name)
  ++ (
    switch (namespace) {
    | None => ""
    | Some(n) => "-" ++ compiledNameSpace(n)
    }
  );

let getName = x =>
  Filename.basename(x) |> Filename.chop_extension |> String.capitalize_ascii;

let filterDuplicates = cmts => {
  /* Remove .cmt's that have .cmti's */
  let intfs = Hashtbl.create(100);
  cmts
  |> List.iter(path =>
       if (Filename.check_suffix(path, ".rei")
           || Filename.check_suffix(path, ".mli")
           || Filename.check_suffix(path, ".cmti")) {
         Hashtbl.add(intfs, getName(path), true);
       }
     );
  cmts
  |> List.filter(path => {
       !(
         (
           Filename.check_suffix(path, ".re")
           || Filename.check_suffix(path, ".rel")
           || Filename.check_suffix(path, ".ml")
           || Filename.check_suffix(path, ".cmt")
         )
         && Hashtbl.mem(intfs, getName(path))
       )
     });
};

let nameSpaceToName = n =>
  n
  |> Str.split(Str.regexp("[-/@]"))
  |> List.map(String.capitalize_ascii)
  |> String.concat("");

let getNamespace = config => {
  let ns = Json.get("namespace", config);
  let isNamespaced =
    ns |?> Json.bool |? (ns |?> Json.string |?> (_ => Some(true)) |? false);
  isNamespaced
    ? ns
      |?> Json.string
      |?? (Json.get("name", config) |?> Json.string)
      |! "name is required if namespace is true"
      |> nameSpaceToName
      |> (s => Some(s))
    : None;
};

let collectFiles = directory => {
  let allFiles = Files.readDirectory(directory);
  let compileds = allFiles |> List.filter(isCompiledFile) |> filterDuplicates;
  let sources = allFiles |> List.filter(isSourceFile) |> filterDuplicates;
  compileds
  |> List.map(path => {
       let modName = getName(path);
       let compiled = directory /+ path;
       let source =
         Utils.find(
           name => getName(name) == modName ? Some(directory /+ name) : None,
           sources,
         );
       (modName, SharedTypes.Impl(compiled, source));
     });
};

/**
 * returns a list of (absolute path to cmt(i), relative path from base to source file)
 */
let findProjectFiles =
    (~debug, namespace, root, sourceDirectories, compiledBase) => {
  let files =
    sourceDirectories
    |> List.map(Infix.fileConcat(root))
    |> ifDebug(debug, "Source directories", String.concat(" - "))
    |> List.map(name => Files.collect(name, isSourceFile))
    |> List.concat
    |> Utils.dedup
    |> ifDebug(debug, "Source files found", String.concat(" : "));

  /* |> filterDuplicates
     |> Utils.filterMap(path => {
       let rel = Files.relpath(root, path);
       ifOneExists([
         compiledBase /+ cmtName(~namespace, rel),
         compiledBase /+ cmiName(~namespace, rel),
       ]) |?>> cm => (cm, path)
     })
     |> ifDebug(debug, "With compiled base", (items) => String.concat("\n", List.map(((a, b)) => a ++ " : " ++ b, items)))
     |> List.filter(((full, rel)) => Files.exists(full))
     /* TODO more than just Impl() */
     |> List.map(((cmt, src)) => (getName(src), SharedTypes.Impl(cmt, Some(src)))) */
  let interfaces = Hashtbl.create(100);
  files
  |> List.iter(path =>
       if (Filename.check_suffix(path, ".rei")
           || Filename.check_suffix(path, ".resi")
           || Filename.check_suffix(path, ".mli")) {
         Log.log("Adding intf " ++ path);
         Hashtbl.replace(interfaces, getName(path), path);
       }
     );

  let normals =
    files
    |> Utils.filterMap(path =>
         if (Filename.check_suffix(path, ".re")
             || Filename.check_suffix(path, ".res")
             || Filename.check_suffix(path, ".rel")
             || Filename.check_suffix(path, ".ml")) {
           let mname = getName(path);
           let intf = Hashtbl.find_opt(interfaces, mname);
           Hashtbl.remove(interfaces, mname);
           let base =
             compiledBaseName(~namespace, Files.relpath(root, path));
           switch (intf) {
           | Some(intf) =>
             let cmti = compiledBase /+ base ++ ".cmti";
             let cmt = compiledBase /+ base ++ ".cmt";
             if (Files.exists(cmti)) {
               if (Files.exists(cmt)) {
                 /* Log.log("Intf and impl " ++ cmti ++ " " ++ cmt); */
                 Some((
                   mname,
                   SharedTypes.IntfAndImpl(cmti, intf, cmt, path),
                 ));
               } else {
                 /* Log.log("Just intf " ++ cmti); */
                 Some((
                   mname,
                   Intf(cmti, intf),
                 ));
               };
             } else {
               Log.log(
                 "Bad source file (no cmt/cmti/cmi) " ++ compiledBase /+ base,
               );
               None;
             };
           | None =>
             let cmt = compiledBase /+ base ++ ".cmt";
             if (Files.exists(cmt)) {
               Some((mname, Impl(cmt, Some(path))));
             } else {
               Log.log(
                 "Bad source file (no cmt/cmi) " ++ compiledBase /+ base,
               );
               None;
             };
           };
         } else {
           Log.log("Bad source file (extension) " ++ path);
           None;
         }
       );

  let result =
    List.append(
      normals,
      Hashtbl.fold(
        (mname, intf, res) => {
          let base = compiledBaseName(~namespace, Files.relpath(root, intf));
          Log.log("Extra intf " ++ intf);
          let cmti = compiledBase /+ base ++ ".cmti";
          if (Files.exists(cmti)) {
            [(mname, SharedTypes.Intf(cmti, intf)), ...res];
          } else {
            res;
          };
        },
        interfaces,
        [],
      ),
    )
    |> List.map(((name, paths)) =>
         switch (namespace) {
         | None => (name, paths)
         | Some(namespace) => (name ++ "-" ++ namespace, paths)
         }
       );

  switch (namespace) {
  | None => result
  | Some(namespace) =>
    let mname = nameSpaceToName(namespace);
    let cmt = compiledBase /+ namespace ++ ".cmt";
    Log.log(
      "adding namespace " ++ namespace ++ " : " ++ mname ++ " : " ++ cmt,
    );
    [(mname, Impl(cmt, None)), ...result];
  };
};

/* let loadStdlib = stdlib => {
     collectFiles(stdlib)
     |> List.filter(((_, (cmt, src))) => Files.exists(cmt))
   }; */

let findDependencyFiles = (~debug, base, config) => {
  let deps =
    config
    |> Json.get("bs-dependencies")
    |?> Json.array
    |? []
    |> optMap(Json.string);
  let devDeps =
    config
    |> Json.get("bs-dev-dependencies")
    |?> Json.array
    |? []
    |> optMap(Json.string);
  let deps = deps @ devDeps;
  Log.log("Deps " ++ String.concat(", ", deps));
  let depFiles =
    deps
    |> List.map(name => {
         let result =
           ModuleResolution.resolveNodeModulePath(~startPath=base, name)
           |?> (
             loc => {
               let innerPath = loc /+ "bsconfig.json";
               Log.log("Dep loc " ++ innerPath);
               switch (Files.readFile(innerPath)) {
               | Some(text) =>
                 let inner = Json.parse(text);
                 let namespace = getNamespace(inner);
                 let directories =
                   getSourceDirectories(~includeDev=false, loc, inner);
                 let%opt compiledBase = BuildSystem.getCompiledBase(loc);
                 /* |! "No compiled base found"; */
                 if (debug) {
                   Log.log("Compiled base: " ++ compiledBase);
                 };
                 let compiledDirectories =
                   directories |> List.map(Infix.fileConcat(compiledBase));
                 let compiledDirectories =
                   namespace == None
                     ? compiledDirectories
                     : [compiledBase, ...compiledDirectories];
                 let files =
                   findProjectFiles(
                     ~debug,
                     namespace,
                     loc,
                     directories,
                     compiledBase,
                   );
                 /* let files =
                    switch (namespace) {
                    | None =>
                       files
                    | Some(namespace) =>
                      files
                      |> List.map(((name, paths)) =>
                           (namespace ++ "-" ++ name, paths)
                         )
                    }; */
                 Some((compiledDirectories, files));
               | None => None
               };
             }
           );

         switch (result) {
         | Some(dependency) => dependency
         | None =>
           Log.log("Skipping nonexistent dependency: " ++ name);
           ([], []);
         };
       });
  let (directories, files) = List.split(depFiles);
  let files = List.concat(files);
  let%try stdlibDirectory = BuildSystem.getStdlib(base);
  let directories = [stdlibDirectory, ...List.concat(directories)];
  let results = files @ collectFiles(stdlibDirectory);
  Ok((directories, results));
};
