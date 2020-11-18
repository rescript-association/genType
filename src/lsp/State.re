open Infix;

open TopTypes;

module Show = {
  let state =
      ({rootPath}, {localModules, dependencyModules, pathsForModule}) => {
    "Root: "
    ++ rootPath
    ++ "\nLocal\n"
    ++ (
      localModules
      |> List.map(name => {
           let paths = Hashtbl.find(pathsForModule, name);
           Printf.sprintf(
             "%s (%s : %s)",
             name,
             SharedTypes.getCmt(paths),
             SharedTypes.getSrc(paths) |? "(no src!)",
           );
         })
      |> String.concat("\n")
    )
    ++ "\nDeps\n"
    ++ (
      dependencyModules
      |> List.map(modname =>
           try({
             let paths = Hashtbl.find(pathsForModule, modname);
             Printf.sprintf(
               "%s (%s : %s)",
               modname,
               SharedTypes.getCmt(paths),
               SharedTypes.getSrc(paths) |? "",
             );
           }) {
           | Not_found => "ERRROR " ++ modname
           }
         )
      |> String.concat("\n")
    );
  };
};

let isMl = path =>
  Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => MarkdownOfOCamldoc.convert(text);
let compose = (fn1, fn2, arg) => fn1(arg) |> fn2;

let converter = (src, usePlainText) => {
  let mlToOutput =
    compose(odocToMd, usePlainText ? Omd.to_text : Omd.to_markdown);
  fold(src, mlToOutput, src =>
    isMl(src)
      ? mlToOutput
      : usePlainText ? compose(Omd.of_string, Omd.to_text) : (x => x)
  );
};

let newDocsForCmt =
    (~moduleName, cmtCache, changed, cmt, src, clientNeedsPlainText) => {
  let uri = Utils.toUri(src |? cmt);
  let%opt file =
    Process_406.fileForCmt(
      ~moduleName,
      cmt,
      uri,
      converter(src, clientNeedsPlainText),
    )
    |> RResult.toOptionAndLog;
  Hashtbl.replace(cmtCache, cmt, (changed, file));
  Some(file);
};

let docsForCmt = (~moduleName, cmt, src, state) =>
  if (Hashtbl.mem(state.cmtCache, cmt)) {
    let (mtime, docs) = Hashtbl.find(state.cmtCache, cmt);
    /* TODO I should really throttle this mtime checking to like every 50 ms or so */
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      if (changed > mtime) {
        newDocsForCmt(
          ~moduleName,
          state.cmtCache,
          changed,
          cmt,
          src,
          state.settings.clientNeedsPlainText,
        );
      } else {
        Some(docs);
      }
    };
  } else {
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      newDocsForCmt(
        ~moduleName,
        state.cmtCache,
        changed,
        cmt,
        src,
        state.settings.clientNeedsPlainText,
      )
    };
  };

let updateContents = (uri, text, version, state) => {
  Hashtbl.remove(state.compiledDocuments, uri);
  Hashtbl.replace(
    state.documentText,
    uri,
    (text, int_of_float(version), false),
  );
  state;
};

let getContents = (uri, state) => {
  let (text, _, _) = Hashtbl.find(state.documentText, uri);
  text;
};

let refmtForUri = (uri, package) =>
  if (Filename.check_suffix(uri, ".ml") || Filename.check_suffix(uri, ".mli")) {
    Ok(None);
  } else if (Filename.check_suffix(uri, ".rel")
             || Filename.check_suffix(uri, ".reli")) {
    switch (package.lispRefmtPath) {
    | None =>
      Error("No lispRefmt path found, cannot process .rel or .reli files")
    | Some(x) => Ok(Some(x))
    };
  } else {
    switch (package.refmtPath) {
    | None =>
      Error("No refmt found for dune project. Cannot process .re file")
    | Some(x) => Ok(Some(x))
    };
  };

let fmtCmdForUri = (~formatWidth, ~interface, uri, package) =>
  if (Filename.check_suffix(uri, ".ml") || Filename.check_suffix(uri, ".mli")) {
    switch (package.mlfmtPath) {
    | None => Error("No formatter available for .ml(i) files")
    | Some(fmtPath) => Ok(fmtPath)
    };
  } else if (Filename.check_suffix(uri, ".rel")
             || Filename.check_suffix(uri, ".reli")) {
    switch (package.lispRefmtPath) {
    | None =>
      Error("No lispRefmt path found, cannot process .rel or .reli files")
    | Some(lispRefmt) =>
      let cmd =
        Printf.sprintf(
          "%s --print re --print-width=%d --parse re%s",
          Commands.shellEscape(lispRefmt),
          formatWidth |? 80,
          interface ? " -i true" : "",
        );
      Ok(cmd);
    };
  } else {
    switch (package.refmtPath) {
    | None =>
      Error("No refmt found for dune project. Cannot process .re file")
    | Some(refmt) =>
      let cmd =
        Printf.sprintf(
          "%s --print re --print-width=%d --parse re%s",
          Commands.shellEscape(refmt),
          formatWidth |? 80,
          interface ? " -i true" : "",
        );
      Ok(cmd);
    };
  };

open Infix;

let getInterfaceFile = (uri, state, ~package: TopTypes.package) => {
  let%try path = Utils.parseUri(uri) |> RResult.orError("Not a uri");
  let text =
    Hashtbl.mem(state.documentText, uri)
      ? {
        let (text, _, _) = Hashtbl.find(state.documentText, uri);
        text;
      }
      : {
        let path = Utils.parseUri(uri) |! "not a uri: " ++ uri;
        Files.readFileExn(path);
      };
  let%try moduleName =
    switch (Hashtbl.find_opt(package.nameForPath, path)) {
    | None =>
      Hashtbl.iter(
        (k, v) => Log.log("Path: " ++ k ++ "  " ++ v),
        package.nameForPath,
      );
      Log.log("Can't find " ++ path);
      Error("Can't find module name for path " ++ path);
    | Some(x) => Ok(x)
    };
  let includes = package.includeDirectories;
  let%try refmtPath = refmtForUri(uri, package);
  AsYouType.getInterface(
    ~moduleName,
    ~rootPath=package.rootPath,
    ~reasonFormat=Utils.endsWith(uri, "re") || Utils.endsWith(uri, "rei"),
    text,
    ~cacheLocation=package.tmpPath,
    package.compilerPath,
    refmtPath,
    includes,
    package.compilationFlags,
  );
};

let getCompilationResult = (uri, state, ~package: TopTypes.package) =>
  if (Hashtbl.mem(state.compiledDocuments, uri)) {
    Ok(Hashtbl.find(state.compiledDocuments, uri));
  } else {
    let%try path = Utils.parseUri(uri) |> RResult.orError("Not a uri");
    let text =
      Hashtbl.mem(state.documentText, uri)
        ? {
          let (text, _, _) = Hashtbl.find(state.documentText, uri);
          text;
        }
        : {
          let path = Utils.parseUri(uri) |! "not a uri: " ++ uri;
          Files.readFileExn(path);
        };
    let moduleName =
      BuildSystem.namespacedName(package.namespace, FindFiles.getName(path));
    /* let%try moduleName = switch (Utils.maybeHash(package.nameForPath, path)) {
         | None =>
           Hashtbl.iter((k, v) => Log.log("Path: " ++ k ++ "  " ++ v), package.nameForPath);
           Log.log("Can't find " ++ path ++ " in package " ++ package.basePath);
           Error("Can't find module name for path " ++ path)
         | Some(x) => Ok(x)
       }; */
    let includes = package.includeDirectories;
    let%try refmtPath = refmtForUri(uri, package);
    let%try result =
      AsYouType.process(
        ~uri,
        ~moduleName,
        ~allLocations=state.settings.recordAllLocations,
        ~rootPath=package.rootPath,
        ~reasonFormat=
          Utils.endsWith(uri, "re") || Utils.endsWith(uri, "rei"),
        text,
        ~cacheLocation=package.tmpPath,
        package.compilerPath,
        refmtPath,
        includes,
        package.compilationFlags,
      );
    Hashtbl.replace(state.compiledDocuments, uri, result);
    switch (result) {
    | AsYouType.SyntaxError(_) => ()
    | AsYouType.TypeError(_, full) =>
      if (!Hashtbl.mem(state.lastDefinitions, uri)) {
        Log.log("<< Making lastDefinitions with type error for " ++ uri);
        Hashtbl.replace(state.lastDefinitions, uri, full);
      }
    | Success(_, full) =>
      Log.log("<< Replacing lastDefinitions for " ++ uri);

      Hashtbl.replace(state.lastDefinitions, uri, full);
      Hashtbl.replace(
        package.interModuleDependencies,
        moduleName,
        SharedTypes.hashList(full.extra.externalReferences) |> List.map(fst),
      );
    };
    Ok(result);
  };

let getFullFromCmt = (uri, state) => {
  let%try path = Utils.parseUri(uri) |> RResult.orError("Not a uri");
  let%try package =
    Packages.getPackage(uri, state, ~reportDiagnostics=(_, _) => ());
  let moduleName =
    BuildSystem.namespacedName(package.namespace, FindFiles.getName(path));
  switch (Hashtbl.find_opt(package.pathsForModule, moduleName)) {
  | Some(paths) =>
    let cmt = SharedTypes.getCmt(~interface=Utils.endsWith(uri, "i"), paths);
    let%try full =
      Process_406.fullForCmt(
        ~moduleName,
        ~allLocations=state.settings.recordAllLocations,
        cmt,
        uri,
        x =>
        x
      );
    Hashtbl.replace(
      package.interModuleDependencies,
      moduleName,
      SharedTypes.hashList(full.extra.externalReferences) |> List.map(fst),
    );
    Ok((package, full));
  | None => Error("can't find module " ++ moduleName)
  };
};

let getLastDefinitions = (uri, state) =>
  switch (Hashtbl.find(state.lastDefinitions, uri)) {
  | exception Not_found => None
  | data => Some(data)
  };

let tryExtra = p => {
  let%try p = p;
  Ok(AsYouType.getResult(p));
};

/* If there's a previous "good" version, use that, otherwise use the current version */
let getBestDefinitions = (uri, state, ~package) =>
  if (Hashtbl.mem(state.lastDefinitions, uri)) {
    Ok(Hashtbl.find(state.lastDefinitions, uri));
  } else {
    tryExtra(getCompilationResult(uri, state, ~package));
  };

let docsForModule = (modname, state, ~package) =>
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let paths = Hashtbl.find(package.pathsForModule, modname);
    /* TODO do better */
    let cmt = SharedTypes.getCmt(paths);
    let src = SharedTypes.getSrc(paths);
    Log.log("FINDING docs for module " ++ SharedTypes.showPaths(paths));
    Log.log("FINDING " ++ cmt ++ " src " ++ (src |? ""));
    let%opt_wrap docs = docsForCmt(~moduleName=modname, cmt, src, state);
    (docs, src);
  } else {
    Log.log("No path for module " ++ modname);
    None;
  };

let fileForUri = (state, ~package, uri) => {
  let%try moduleData = getCompilationResult(uri, state, ~package) |> tryExtra;
  Ok((moduleData.file, moduleData.extra));
};

let fileForModule = (state, ~package, modname) => {
  let%opt (file, _) = docsForModule(modname, state, ~package);
  Some(file);
};

let extraForModule = (state, ~package, modname) =>
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let paths = Hashtbl.find(package.pathsForModule, modname);
    /* TODO do better? */
    let%opt src = SharedTypes.getSrc(paths);
    let%opt {extra} =
      tryExtra(getCompilationResult(Utils.toUri(src), state, ~package))
      |> RResult.toOptionAndLog;
    Some(extra);
  } else {
    None;
  };
