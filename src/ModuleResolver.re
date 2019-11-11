open GenTypeCommon;

module ModuleNameMap = Map.Make(ModuleName);

/* Read all the dirs from a library in node_modules */
let readLibraryDirs = (~root) => {
  let dirs = ref([]);
  let rec findSubDirs = dir => {
    let absDir = dir == "" ? root : Filename.concat(root, dir);
    if (Sys.is_directory(absDir) && Sys.file_exists(absDir)) {
      dirs := [dir, ...dirs^];
      absDir
      |> Sys.readdir
      |> Array.iter(d => findSubDirs(Filename.concat(dir, d)));
    };
  };
  findSubDirs("");
  dirs^;
};

let readSourceDirs = () => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"]
    |> List.fold_left(Filename.concat, projectRoot^);

  let getDirs = json => {
    let dirs = ref([]);
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      switch (map |> String_map.find_opt("dirs")) {
      | Some(Arr({content, _})) =>
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str({str, _}) => dirs := [str, ...dirs^]
             | _ => ()
             }
           );
        ();
      | _ => ()
      }
    | _ => ()
    };
    dirs^;
  };

  if (sourceDirs |> Sys.file_exists) {
    try(sourceDirs |> Ext_json_parse.parse_json_from_file |> getDirs) {
    | _ => []
    };
  } else {
    [];
  };
};

/* Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong.  */
let sourcedirsJsonToMap = (~config, ~extensions, ~excludeFile) => {
  let rec chopExtensions = fname =>
    switch (fname |> Filename.chop_extension) {
    | fnameChopped => fnameChopped |> chopExtensions
    | exception _ => fname
    };

  let fileMap = ref(ModuleNameMap.empty);
  let librariesCmtMap = ref(ModuleNameMap.empty);

  let filterGivenExtension = fileName =>
    extensions
    |> List.exists(ext => Filename.check_suffix(fileName, ext))
    && !excludeFile(fileName);

  let addDir = (~filter, ~map, ~root, dir) =>
    dir
    |> Filename.concat(root)
    |> Sys.readdir
    |> Array.iter(fname =>
         if (fname |> filter) {
           map :=
             map^
             |> ModuleNameMap.add(
                  fname |> chopExtensions |> ModuleName.fromStringUnsafe,
                  dir,
                );
         }
       );
  readSourceDirs()
  |> List.iter(
       addDir(~filter=filterGivenExtension, ~map=fileMap, ~root=projectRoot^),
     );

  config.bsDependencies
  |> List.iter(s => {
       let root =
         ["node_modules", s, "lib", "bs"]
         |> List.fold_left(Filename.concat, projectRoot^);
       let filter = fileName =>
         [".cmt", ".cmti"]
         |> List.exists(ext => Filename.check_suffix(fileName, ext));
       readLibraryDirs(~root)
       |> List.iter(dir =>
            [s, "lib", "bs", dir]
            /* TODO: add upstream a new form of implicit paths into node_modules */
            |> List.fold_left(Filename.concat, "node_modules")
            |> addDir(~filter, ~map=librariesCmtMap, ~root=projectRoot^)
          );
     });

  (fileMap^, librariesCmtMap^);
};

type case =
  | Lowercase
  | Uppercase;

type resolver = {
  lazyFind:
    Lazy.t((~useLibraries: bool, ModuleName.t) => option((string, case))),
};

let createResolver = (~config, ~extensions, ~excludeFile) => {
  lazyFind:
    lazy({
      let (moduleNameMap, librariesCmtMap) =
        sourcedirsJsonToMap(~config, ~extensions, ~excludeFile);
      let find = (~map, moduleName) =>
        switch (map |> ModuleNameMap.find(moduleName)) {
        | resovedModuleDir => Some((resovedModuleDir, Uppercase))
        | exception Not_found =>
          switch (
            map |> ModuleNameMap.find(moduleName |> ModuleName.uncapitalize)
          ) {
          | resovedModuleDir => Some((resovedModuleDir, Lowercase))
          | exception Not_found => None
          }
        };
      (~useLibraries, moduleName) =>
        switch (moduleName |> find(~map=moduleNameMap)) {
        | None when useLibraries => moduleName |> find(~map=librariesCmtMap)
        | res => res
        };
    }),
};

let apply = (~resolver, ~useLibraries, moduleName) =>
  moduleName |> Lazy.force(resolver.lazyFind, ~useLibraries);

/* Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".re" or ".js". */
let resolveModule =
    (
      ~importExtension,
      ~outputFileRelative,
      ~resolver,
      ~useLibraries,
      moduleName,
    ) => {
  open Filename;

  let outputFileRelativeDir =
    /* e.g. src if we're generating src/File.re.js */
    dirname(outputFileRelative);
  let outputFileAbsoluteDir = concat(projectRoot^, outputFileRelativeDir);
  let moduleNameReFile =
    /* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.re exists. */
    concat(outputFileAbsoluteDir, ModuleName.toString(moduleName) ++ ".re");
  let candidate =
    /* e.g. import "./Modulename.ext" */
    moduleName
    |> ImportPath.fromModule(~dir=current_dir_name, ~importExtension);
  if (Sys.file_exists(moduleNameReFile)) {
    candidate;
  } else {
    let rec pathToList = path => {
      let isRoot = path |> basename == path;
      isRoot ? [path] : [path |> basename, ...path |> dirname |> pathToList];
    };
    switch (moduleName |> apply(~resolver, ~useLibraries)) {
    | None => candidate
    | Some((resovedModuleDir, case)) =>
      /* e.g. "dst" in case of dst/ModuleName.re */

      let walkUpOutputDir =
        /* e.g. ".." in case dst is a path of length 1 */
        outputFileRelativeDir
        |> pathToList
        |> List.map(_ => parent_dir_name)
        |> (
          l =>
            switch (l) {
            | [] => ""
            | [_, ...rest] => rest |> List.fold_left(concat, parent_dir_name)
            }
        );

      let fromOutputDirToModuleDir =
        /* e.g. "../dst" */
        concat(walkUpOutputDir, resovedModuleDir);

      /* e.g. import "../dst/ModuleName.ext" */
      (case == Uppercase ? moduleName : moduleName |> ModuleName.uncapitalize)
      |> ImportPath.fromModule(
           ~dir=fromOutputDirToModuleDir,
           ~importExtension,
         );
    };
  };
};

let resolveGeneratedModule =
    (~config, ~outputFileRelative, ~resolver, moduleName) => {
  if (Debug.moduleResolution^) {
    logItem(
      "Resolve Generated Module: %s\n",
      moduleName |> ModuleName.toString,
    );
  };
  let importPath =
    resolveModule(
      ~importExtension=EmitType.generatedModuleExtension(~config),
      ~outputFileRelative,
      ~resolver,
      ~useLibraries=true,
      moduleName,
    );
  if (Debug.moduleResolution^) {
    logItem("Import Path: %s\n", importPath |> ImportPath.dump);
  };
  importPath;
};

/**
 * Returns the path to import a given Reason module name.
 */
let importPathForReasonModuleName =
    (~config, ~outputFileRelative, ~resolver, moduleName) => {
  if (Debug.moduleResolution^) {
    logItem("Resolve Reason Module: %s\n", moduleName |> ModuleName.toString);
  };
  switch (config.shimsMap |> ModuleNameMap.find(moduleName)) {
  | shimModuleName =>
    if (Debug.moduleResolution^) {
      logItem("ShimModuleName: %s\n", shimModuleName |> ModuleName.toString);
    };
    let importPath =
      resolveModule(
        ~importExtension=".shim",
        ~outputFileRelative,
        ~resolver,
        ~useLibraries=false,
        shimModuleName,
      );
    if (Debug.moduleResolution^) {
      logItem("Import Path: %s\n", importPath |> ImportPath.dump);
    };
    importPath;
  | exception Not_found =>
    moduleName
    |> resolveGeneratedModule(~config, ~outputFileRelative, ~resolver)
  };
};