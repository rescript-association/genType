open GenTypeCommon;

module ModuleNameMap = Map.Make(ModuleName);

let (+++) = Filename.concat;

/* Read all the dirs from a library in node_modules */
let readBsDependenciesDirs = (~root) => {
  let dirs = ref([]);
  let rec findSubDirs = dir => {
    let absDir = dir == "" ? root : root +++ dir;
    if (Sys.file_exists(absDir) && Sys.is_directory(absDir)) {
      dirs := [dir, ...dirs^];
      absDir |> Sys.readdir |> Array.iter(d => findSubDirs(dir +++ d));
    };
  };
  findSubDirs("");
  dirs^;
};

type pkgs = {
  dirs: list(string),
  pkgs: Hashtbl.t(string, string),
};

let readSourceDirs = () => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"] |> List.fold_left((+++), projectRoot^);
  let dirs = ref([]);
  let pkgs = Hashtbl.create(1);

  let getDirsPkgs = json => {
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
      };
      switch (map |> String_map.find_opt("pkgs")) {
      | Some(Arr({content})) =>
        content
        |> Array.iter(x =>
             switch (x) {
             | Ext_json_types.Arr({
                 content: [|Str({str: name}), Str({str: path})|],
               }) =>
               Hashtbl.add(pkgs, name, path)
             | _ => ()
             }
           );
        ();
      | _ => ()
      };
    | _ => ()
    };
  };

  if (sourceDirs |> Sys.file_exists) {
    try(sourceDirs |> Ext_json_parse.parse_json_from_file |> getDirsPkgs) {
    | _ => ()
    };
  };
  {dirs: dirs^, pkgs};
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
  let bsDependenciesFileMap = ref(ModuleNameMap.empty);

  let filterGivenExtension = fileName =>
    extensions
    |> List.exists(ext => Filename.check_suffix(fileName, ext))
    && !excludeFile(fileName);

  let addDir = (~dirOnDisk, ~dirEmitted, ~filter, ~map, ~root) => {
    dirOnDisk
    |> Sys.readdir
    |> Array.iter(fname =>
         if (fname |> filter) {
           map :=
             map^
             |> ModuleNameMap.add(
                  fname |> chopExtensions |> ModuleName.fromStringUnsafe,
                  dirEmitted,
                );
         }
       );
  };
  let {dirs, pkgs} = readSourceDirs();
  dirs
  |> List.iter(dir =>
       addDir(
         ~dirEmitted=dir,
         ~dirOnDisk=projectRoot^ +++ dir,
         ~filter=filterGivenExtension,
         ~map=fileMap,
         ~root=projectRoot^,
       )
     );

  if (config.useBsDependencies) {
    config.bsDependencies
    |> List.iter(packageName => {
         switch (Hashtbl.find(pkgs, packageName)) {
         | path =>
           let root = ["lib", "bs"] |> List.fold_left((+++), path);
           let filter = fileName =>
             [".cmt", ".cmti"]
             |> List.exists(ext => Filename.check_suffix(fileName, ext));
           readBsDependenciesDirs(~root)
           |> List.iter(dir => {
                let dirOnDisk = root +++ dir;
                let dirEmitted = packageName +++ dir;
                addDir(
                  ~dirEmitted,
                  ~dirOnDisk,
                  ~filter,
                  ~map=bsDependenciesFileMap,
                  ~root=projectRoot^,
                );
              });
         | exception Not_found => ()
         }
       });
  };

  (fileMap^, bsDependenciesFileMap^);
};

type case =
  | Lowercase
  | Uppercase;

type resolver = {
  lazyFind:
    Lazy.t(
      (~useBsDependencies: bool, ModuleName.t) =>
      option((string, case, bool)),
    ),
};

let createResolver = (~config, ~extensions, ~excludeFile) => {
  lazyFind:
    lazy({
      let (moduleNameMap, bsDependenciesFileMap) =
        sourcedirsJsonToMap(~config, ~extensions, ~excludeFile);
      let find = (~bsDependencies, ~map, moduleName) =>
        switch (map |> ModuleNameMap.find(moduleName)) {
        | resolvedModuleDir =>
          Some((resolvedModuleDir, Uppercase, bsDependencies))
        | exception Not_found =>
          switch (
            map |> ModuleNameMap.find(moduleName |> ModuleName.uncapitalize)
          ) {
          | resolvedModuleDir =>
            Some((resolvedModuleDir, Lowercase, bsDependencies))
          | exception Not_found => None
          }
        };
      (~useBsDependencies, moduleName) =>
        switch (moduleName |> find(~bsDependencies=false, ~map=moduleNameMap)) {
        | None when useBsDependencies =>
          moduleName |> find(~bsDependencies=true, ~map=bsDependenciesFileMap)
        | res => res
        };
    }),
};

let apply = (~resolver, ~useBsDependencies, moduleName) =>
  moduleName |> Lazy.force(resolver.lazyFind, ~useBsDependencies);

/* Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".re" or ".js". */
let resolveModule =
    (
      ~importExtension,
      ~outputFileRelative,
      ~resolver,
      ~useBsDependencies,
      moduleName,
    ) => {
  let outputFileRelativeDir =
    /* e.g. src if we're generating src/File.re.js */
    Filename.dirname(outputFileRelative);
  let outputFileAbsoluteDir = projectRoot^ +++ outputFileRelativeDir;
  let moduleNameReFile =
    /* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.re exists. */
    outputFileAbsoluteDir +++ ModuleName.toString(moduleName) ++ ".re";
  let candidate =
    /* e.g. import "./Modulename.ext" */
    moduleName
    |> ImportPath.fromModule(~dir=Filename.current_dir_name, ~importExtension);
  if (Sys.file_exists(moduleNameReFile)) {
    candidate;
  } else {
    let rec pathToList = path => {
      let isRoot = path |> Filename.basename == path;
      isRoot
        ? [path]
        : [
          path |> Filename.basename,
          ...path |> Filename.dirname |> pathToList,
        ];
    };
    switch (moduleName |> apply(~resolver, ~useBsDependencies)) {
    | None => candidate
    | Some((resolvedModuleDir, case, isLibrary)) =>
      /* e.g. "dst" in case of dst/ModuleName.re */

      let walkUpOutputDir =
        /* e.g. ".." in case dst is a path of length 1 */
        outputFileRelativeDir
        |> pathToList
        |> List.map(_ => Filename.parent_dir_name)
        |> (
          l =>
            switch (l) {
            | [] => ""
            | [_, ...rest] =>
              rest |> List.fold_left((+++), Filename.parent_dir_name)
            }
        );

      let fromOutputDirToModuleDir =
        /* e.g. "../dst" */
        isLibrary ? resolvedModuleDir : walkUpOutputDir +++ resolvedModuleDir;

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
      ~useBsDependencies=true,
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
        ~useBsDependencies=false,
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