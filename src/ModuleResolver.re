open GenFlowCommon;

module ModuleNameMap = Map.Make(ModuleName);

/* Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong.  */
let sourcedirsJsonToMap = (~extensions) => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"]
    |> List.fold_left(Filename.concat, Paths.projectRoot^);

  let getDirs = json => {
    let dirs = ref([]);
    switch (json) {
    | Ext_json_types.Obj({map}) =>
      switch (map |> String_map.find_opt("dirs")) {
      | Some(Arr({content})) =>
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Str({str}) => dirs := [str, ...dirs^]
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

  let rec chopExtensions = fname =>
    switch (fname |> Filename.chop_extension) {
    | fnameChopped => fnameChopped |> chopExtensions
    | exception _ => fname
    };

  let createFileMap = (~filter, dirs) => {
    let fileMap = ref(ModuleNameMap.empty);
    dirs
    |> List.iter(dir =>
         dir
         |> Filename.concat(Paths.projectRoot^)
         |> Sys.readdir
         |> Array.iter(fname =>
              if (fname |> filter) {
                fileMap :=
                  fileMap^
                  |> ModuleNameMap.add(
                       fname |> chopExtensions |> ModuleName.fromString,
                       dir,
                     );
              }
            )
       );
    fileMap^;
  };

  if (sourceDirs |> Sys.file_exists) {
    try (
      sourceDirs
      |> Ext_json_parse.parse_json_from_file
      |> getDirs
      |> createFileMap(~filter=fileName =>
           extensions
           |> List.exists(ext => Filename.check_suffix(fileName, ext))
         )
    ) {
    | _ => ModuleNameMap.empty
    };
  } else {
    ModuleNameMap.empty;
  };
};

type resolver = {lazyFind: Lazy.t(ModuleName.t => option(string))};

let createResolver = (~extensions) => {
  lazyFind:
    lazy {
      let map = sourcedirsJsonToMap(~extensions);
      moduleName =>
        switch (map |> ModuleNameMap.find(moduleName)) {
        | resolvedModuleName => Some(resolvedModuleName)
        | exception _ => None
        };
    },
};

let apply = (~resolver, moduleName) =>
  Lazy.force(resolver.lazyFind, moduleName);

/* Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".re" or ".js". */
let resolveModule = (~outputFileRelative, ~resolver, ~ext, moduleName) => {
  open Filename;
  let moduleNameString = moduleName |> ModuleName.toString;
  let outputFileRelativeDir =
    /* e.g. src if we're generating src/File.re.js */
    dirname(outputFileRelative);
  let outputFileAbsoluteDir =
    concat(Paths.projectRoot^, outputFileRelativeDir);
  let moduleNameReFile =
    /* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.re exists. */
    concat(outputFileAbsoluteDir, ModuleName.toString(moduleName) ++ ".re");
  if (Sys.file_exists(moduleNameReFile)) {
    /* e.g. import "./Modulename.ext" */
    concat(
      current_dir_name,
      moduleNameString ++ ext,
    );
  } else {
    let candidate = concat(current_dir_name, moduleNameString ++ ext);
    let rec pathToList = path => {
      let isRoot = path |> basename == path;
      isRoot ? [path] : [path |> basename, ...path |> dirname |> pathToList];
    };
    switch (moduleName |> apply(~resolver)) {
    | None => candidate
    | Some(resovedModuleDir) =>
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

      let resolvedImportPath =
        /* e.g. import "../dst/ModuleName.ext" */
        concat(
          fromOutputDirToModuleDir,
          ModuleName.toString(moduleName) ++ ext,
        );

      resolvedImportPath;
    };
  };
};

let resolveSourceModule = (~outputFileRelative, ~resolver, moduleName) => {
  if (Debug.moduleResolution) {
    logItem("Resolve Source Module: %s\n", moduleName |> ModuleName.toString);
  };
  let importPath =
    resolveModule(~outputFileRelative, ~resolver, ~ext=".bs", moduleName);
  if (Debug.moduleResolution) {
    logItem("Import Path: %s\n", importPath);
  };
  importPath;
};

let resolveGeneratedModule = (~outputFileRelative, ~resolver, moduleName) => {
  if (Debug.moduleResolution) {
    logItem(
      "Resolve Generated Module: %s\n",
      moduleName |> ModuleName.toString,
    );
  };
  let importPath =
    resolveModule(~outputFileRelative, ~resolver, ~ext=".re", moduleName);
  if (Debug.moduleResolution) {
    logItem("Import Path: %s\n", importPath);
  };
  importPath;
};

/**
 * Returns the path to import a given Reason module name.
 */
let importPathForReasonModuleName =
    (~outputFileRelative, ~resolver, ~modulesMap, ~moduleName) => {
  if (Debug.moduleResolution) {
    logItem("Resolve Reason Module: %s\n", moduleName |> ModuleName.toString);
  };
  switch (modulesMap |> ModuleNameMap.find(moduleName)) {
  | shimModuleName =>
    if (Debug.moduleResolution) {
      logItem("ShimModuleName: %s\n", shimModuleName);
    };
    let importPath =
      resolveModule(
        ~outputFileRelative,
        ~resolver,
        ~ext=".shim.js",
        shimModuleName |> ModuleName.fromString,
      );
    if (Debug.moduleResolution) {
      logItem("Import Path: %s\n", importPath);
    };
    importPath;
  | exception Not_found =>
    moduleName |> resolveGeneratedModule(~outputFileRelative, ~resolver)
  };
};