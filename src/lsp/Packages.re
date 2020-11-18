open Infix;
open TopTypes;
// open BuildCommand;

let escapePreprocessingFlags = flag =>
  /* ppx escaping not supported on windows yet */
  if (Sys.os_type == "Win32") {
    flag;
  } else {
    let parts = Utils.split_on_char(' ', flag);
    switch (parts) {
    | [("-ppx" | "-pp") as flag, ...rest] =>
      flag ++ " " ++ Utils.maybeQuoteFilename(String.concat(" ", rest))
    | _ => flag
    };
  };

/**
 * Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files)
 */
let makePathsForModule =
    (
      localModules: list((string, SharedTypes.paths)),
      dependencyModules: list((string, SharedTypes.paths)),
    ) => {
  let pathsForModule = Hashtbl.create(30);
  let nameForPath = Hashtbl.create(30);
  let add = (name, paths) =>
    switch (paths) {
    | SharedTypes.Intf(_, path) => Hashtbl.replace(nameForPath, path, name)
    | SharedTypes.Impl(_, Some(path)) =>
      Hashtbl.replace(nameForPath, path, name)
    | SharedTypes.IntfAndImpl(_, intf, _, impl) =>
      Hashtbl.replace(nameForPath, intf, name);
      Hashtbl.replace(nameForPath, impl, name);
    | _ => ()
    };

  dependencyModules
  |> List.iter(((modName, paths)) => {
       add(modName, paths);
       Hashtbl.replace(pathsForModule, modName, paths);
     });

  localModules
  |> List.iter(((modName, paths)) => {
       add(modName, paths);
       Hashtbl.replace(pathsForModule, modName, paths);
     });

  (pathsForModule, nameForPath);
};

let newBsPackage = (~reportDiagnostics, state, rootPath) => {
  let%try raw = Files.readFileResult(rootPath /+ "bsconfig.json");
  let config = Json.parse(raw);

  let%try bsPlatform = BuildSystem.getBsPlatformDir(rootPath);

  let bsb =
    switch (Files.ifExists(bsPlatform /+ "lib" /+ "bsb.exe")) {
    | Some(x) => x
    | None =>
      switch (
        Files.ifExists(
          bsPlatform /+ Lazy.force(BuildSystem.nodePlatform) /+ "bsb.exe",
        )
      ) {
      | Some(x) => x
      | None => failwith("can not locate bsb.exe in " ++ bsPlatform)
      }
    };

  let buildCommand = bsb ++ " -make-world";

  Log.log({|📣 📣 NEW BSB PACKAGE 📣 📣|});
  /* failwith("Wat"); */
  Log.log("- location: " ++ rootPath);
  Log.log("- bsPlatform: " ++ bsPlatform);
  Log.log("- build command: " ++ buildCommand);

  let%try () =
    if (state.settings.autoRebuild) {
      BuildCommand.runBuildCommand(
        ~reportDiagnostics,
        ~state,
        ~rootPath,
        Some(buildCommand),
      );
    } else {
      Ok();
    };

  let compiledBase = BuildSystem.getCompiledBase(rootPath);
  let%try stdLibDirectory = BuildSystem.getStdlib(rootPath);
  let%try compilerPath = BuildSystem.getCompiler(rootPath);
  let mlfmtPath = state.settings.mlfmtLocation;
  let%try refmtPath = BuildSystem.getRefmt(rootPath);
  let%try tmpPath = BuildSystem.hiddenLocation(rootPath);
  let%try (dependencyDirectories, dependencyModules) =
    FindFiles.findDependencyFiles(~debug=true, rootPath, config);
  let%try_wrap compiledBase =
    compiledBase
    |> RResult.orError(
         "You need to run bsb first so that reason-language-server can access the compiled artifacts.\nOnce you've run bsb, restart the language server.",
       );

  let namespace = FindFiles.getNamespace(config);
  let localSourceDirs =
    FindFiles.getSourceDirectories(~includeDev=true, rootPath, config);
  Log.log(
    "Got source directories " ++ String.concat(" - ", localSourceDirs),
  );
  let localCompiledDirs =
    localSourceDirs |> List.map(Infix.fileConcat(compiledBase));
  let localCompiledDirs =
    namespace == None
      ? localCompiledDirs : [compiledBase, ...localCompiledDirs];

  let localModules =
    FindFiles.findProjectFiles(
      ~debug=true,
      namespace,
      rootPath,
      localSourceDirs,
      compiledBase,
    );
  /* |> List.map(((name, paths)) => (switch (namespace) {
     | None => name
     | Some(n) => name ++ "-" ++ n }, paths)); */
  Log.log(
    "-- All local modules found: "
    ++ string_of_int(List.length(localModules)),
  );
  localModules
  |> List.iter(((name, paths)) => {
       Log.log(name);
       switch (paths) {
       | SharedTypes.Impl(cmt, _) => Log.log("impl " ++ cmt)
       | Intf(cmi, _) => Log.log("intf " ++ cmi)
       | _ => Log.log("Both")
       };
     });

  let (pathsForModule, nameForPath) =
    makePathsForModule(localModules, dependencyModules);

  let opens =
    switch (namespace) {
    | None => []
    | Some(namespace) =>
      let cmt = compiledBase /+ namespace ++ ".cmt";
      Log.log("############ Namespaced as " ++ namespace ++ " at " ++ cmt);
      Hashtbl.add(pathsForModule, namespace, Impl(cmt, None));
      [FindFiles.nameSpaceToName(namespace)];
    };
  Log.log("Dependency dirs " ++ String.concat(" ", dependencyDirectories));

  let (flags, opens) = {
    let flags =
      MerlinFile.getFlags(rootPath)
      |> RResult.withDefault([""])
      |> List.map(escapePreprocessingFlags);
    let opens =
      List.fold_left(
        (opens, item) => {
          let parts = Utils.split_on_char(' ', item);
          let rec loop = items =>
            switch (items) {
            | ["-open", name, ...rest] => [name, ...loop(rest)]
            | [_, ...rest] => loop(rest)
            | [] => []
            };
          opens @ loop(parts);
        },
        opens,
        flags,
      );
    (flags, opens);
  };

  let flags = {
    let jsPackageMode =
      {
        let specs = config |> Json.get("package-specs");
        let spec =
          switch (specs) {
          | Some(Json.Array([item, ..._])) => Some(item)
          | Some(Json.Object(_)) => specs
          | _ => None
          };
        spec |?> Json.get("module") |?> Json.string;
      }
      |? "commonjs";
    let flags =
      switch (jsPackageMode) {
      | "es6" as packageMode
      | "es6-global" as packageMode => [
          "-bs-package-name",
          config |> Json.get("name") |?> Json.string |? "unnamed",
          ...packageMode == "es6"
               ? ["-bs-package-output", "es6:node_modules/.lsp", ...flags]
               : flags,
        ]
      | _ => flags
      };
    /* flags */
    ["-bs-no-builtin-ppx", ...flags];
  };

  let interModuleDependencies = Hashtbl.create(List.length(localModules));

  {
    rootPath,
    rebuildTimer: 0.,
    localModules: localModules |> List.map(fst),
    dependencyModules: dependencyModules |> List.map(fst),
    pathsForModule,
    nameForPath,
    buildCommand: state.settings.autoRebuild ? Some(buildCommand) : None,
    opens,
    tmpPath,
    namespace,
    compilationFlags: flags |> String.concat(" "),
    interModuleDependencies,
    includeDirectories:
      localCompiledDirs @ dependencyDirectories @ [stdLibDirectory],

    compilerPath,
    mlfmtPath,
    refmtPath: Some(refmtPath),
    /*** TODO detect this from node_modules */
    lispRefmtPath: None,
  };
};

let findRoot = (uri, packagesByRoot) => {
  let%opt path = Utils.parseUri(uri);
  let rec loop = path =>
    if (path == "/") {
      None;
    } else if (Hashtbl.mem(packagesByRoot, path)) {
      Some(`Root(path));
    } else if (Files.exists(path /+ "bsconfig.json")) {
      Some(`Bs(path));
    } else {
      loop(Filename.dirname(path));
    };
  loop(Filename.dirname(path));
};

let getPackage = (~reportDiagnostics, uri, state) =>
  if (Hashtbl.mem(state.rootForUri, uri)) {
    Ok(
      Hashtbl.find(
        state.packagesByRoot,
        Hashtbl.find(state.rootForUri, uri),
      ),
    );
  } else {
    let%try root =
      findRoot(uri, state.packagesByRoot)
      |> RResult.orError("No root directory found");
    let%try package =
      switch (root) {
      | `Root(rootPath) =>
        Hashtbl.replace(state.rootForUri, uri, rootPath);
        Ok(
          Hashtbl.find(
            state.packagesByRoot,
            Hashtbl.find(state.rootForUri, uri),
          ),
        );
      | `Bs(rootPath) =>
        let%try package = newBsPackage(~reportDiagnostics, state, rootPath);
        Files.mkdirp(package.tmpPath);
        let package = {
          ...package,
          refmtPath: state.settings.refmtLocation |?? package.refmtPath,
          lispRefmtPath:
            state.settings.lispRefmtLocation |?? package.lispRefmtPath,
        };
        Hashtbl.replace(state.rootForUri, uri, package.rootPath);
        Hashtbl.replace(state.packagesByRoot, package.rootPath, package);
        Ok(package);
      };

    Ok(package);
  };
