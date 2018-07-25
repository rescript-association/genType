module StringMap = Map.Make(String);

/* Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong.  */
let sourcedirsJsonToMap = (~projectRoot, ~ext) => {
  let sourceDirs =
    ["lib", "bs", ".sourcedirs.json"]
    |> List.fold_left(Filename.concat, projectRoot);

  let getDirs = json => {
    let dirs = ref([]);
    switch (json) {
    | Ext_json_types.Obj({map}) when map |> String_map.mem("dirs") =>
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

  let createFileMap = (~filter, dirs) => {
    let fileMap = ref(StringMap.empty);
    dirs
    |> List.iter(dir =>
         dir
         |> Filename.concat(projectRoot)
         |> Sys.readdir
         |> Array.iter(fname =>
              if (fname |> filter) {
                fileMap :=
                  fileMap^
                  |> StringMap.add(Filename.chop_extension(fname), dir);
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
           Filename.check_suffix(fileName, ext)
         )
    ) {
    | _ => StringMap.empty
    };
  } else {
    StringMap.empty;
  };
};

type resolver = {
  lazyFind: Lazy.t(string => option(string)),
  projectRoot: string,
};

let createResolver = (~projectRoot, ~ext) => {
  lazyFind:
    lazy {
      let map = sourcedirsJsonToMap(~projectRoot, ~ext);
      moduleName =>
        switch (map |> StringMap.find(moduleName)) {
        | resolvedModuleName => Some(resolvedModuleName)
        | exception _ => None
        };
    },
  projectRoot,
};

let apply = (~resolver, moduleName) =>
  Lazy.force(resolver.lazyFind, moduleName);

/* Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".re" or ".js". */
let resolveModule = (~outputFileRelative, ~resolver, ~ext, moduleName) => {
  open Filename;
  let outputFileRelativeDir =
    /* e.g. src if we're generating src/File.re.js */
    dirname(outputFileRelative);
  let outputFileAbsoluteDir =
    concat(resolver.projectRoot, outputFileRelativeDir);
  let moduleNameReFile =
    /* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.re exists. */
    concat(outputFileAbsoluteDir, moduleName ++ ".re");
  if (Sys.file_exists(moduleNameReFile)) {
    /* e.g. import "./Modulename.ext" */
    concat(
      current_dir_name,
      moduleName ++ ext,
    );
  } else {
    let candidate = concat(current_dir_name, moduleName ++ ext);
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
        concat(fromOutputDirToModuleDir, moduleName ++ ext);

      /* print_endline(
           "resolveModule "
           ++ moduleName
           ++ "\n  outputFileRelativeDir: "
           ++ outputFileRelativeDir
           ++ "\n  resovedModuleDir: "
           ++ resovedModuleDir
           ++ "\n  outputFileRelative: "
           ++ outputFileRelative
           ++ "\n  walkUpOutputDir: "
           ++ walkUpOutputDir
           ++ "\n  fromOutputDirToModuleDir: "
           ++ fromOutputDirToModuleDir
           ++ "\n  resolvedImportPath: "
           ++ resolvedImportPath,
         ); */

      resolvedImportPath;
    };
  };
};

let resolveSourceModule = (~outputFileRelative, ~resolver, moduleName) =>
  resolveModule(~outputFileRelative, ~resolver, ~ext=".bs", moduleName);

let resolveGeneratedModule = (~outputFileRelative, ~resolver, moduleName) =>
  resolveModule(~outputFileRelative, ~resolver, ~ext=".re", moduleName);