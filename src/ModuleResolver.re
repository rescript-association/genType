open GenTypeCommon;

module ModuleNameMap = Map.Make(ModuleName);

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
    try (sourceDirs |> Ext_json_parse.parse_json_from_file |> getDirs) {
    | _ => []
    };
  } else {
    [];
  };
};

/* Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong.  */
let sourcedirsJsonToMap = (~extensions, ~excludeFile) => {
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
         |> Filename.concat(projectRoot^)
         |> Sys.readdir
         |> Array.iter(fname =>
              if (fname |> filter) {
                fileMap :=
                  fileMap^
                  |> ModuleNameMap.add(
                       fname |> chopExtensions |> ModuleName.fromStringUnsafe,
                       dir,
                     );
              }
            )
       );
    fileMap^;
  };

  readSourceDirs()
  |> createFileMap(~filter=fileName =>
       extensions
       |> List.exists(ext => Filename.check_suffix(fileName, ext))
       && !excludeFile(fileName)
     );
};

type resolver = {lazyFind: Lazy.t(ModuleName.t => option(string))};

let createResolver = (~extensions, ~excludeFile) => {
  lazyFind:
    lazy {
      let map = sourcedirsJsonToMap(~extensions, ~excludeFile);
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
let resolveModule =
    (~config, ~outputFileRelative, ~resolver, ~importExtension, moduleName) => {
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
    |> ImportPath.fromModule(~config, ~dir=current_dir_name, ~importExtension);
  if (Sys.file_exists(moduleNameReFile)) {
    candidate;
  } else {
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

      /* e.g. import "../dst/ModuleName.ext" */
      moduleName
      |> ImportPath.fromModule(
           ~config,
           ~dir=fromOutputDirToModuleDir,
           ~importExtension,
         );
    };
  };
};

let resolveSourceModule = (~importPath, moduleName) => {
  if (Debug.moduleResolution^) {
    logItem("Resolve Source Module: %s\n", moduleName |> ModuleName.toString);
  };
  if (Debug.moduleResolution^) {
    logItem("Import Path: %s\n", importPath |> ImportPath.toString);
  };
  importPath;
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
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~importExtension=EmitType.generatedModuleExtension(~config),
      moduleName,
    );
  if (Debug.moduleResolution^) {
    logItem("Import Path: %s\n", importPath |> ImportPath.toString);
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
  switch (config.modulesMap |> ModuleNameMap.find(moduleName)) {
  | shimModuleName =>
    if (Debug.moduleResolution^) {
      logItem("ShimModuleName: %s\n", shimModuleName |> ModuleName.toString);
    };
    let importPath =
      resolveModule(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~importExtension=".shim",
        shimModuleName,
      );
    if (Debug.moduleResolution^) {
      logItem("Import Path: %s\n", importPath |> ImportPath.toString);
    };
    importPath;
  | exception Not_found =>
    moduleName
    |> resolveGeneratedModule(~config, ~outputFileRelative, ~resolver)
  };
};