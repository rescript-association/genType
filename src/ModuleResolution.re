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

let createResolver = (~projectRoot, ~ext) =>
  lazy {
    let map = sourcedirsJsonToMap(~projectRoot, ~ext);
    moduleName =>
      switch (map |> StringMap.find(moduleName)) {
      | resolvedModuleName => Some(resolvedModuleName)
      | exception _ => None
      };
  };

let resolveModule = (~outputFileRelative, ~resolver, ~ext, moduleName) => {
  open Filename;
  /* TODO: find the path from the module name */
  let candidate = concat(current_dir_name, moduleName ++ ext);
  let resolved =
    switch (moduleName |> Lazy.force(resolver)) {
    | None => candidate
    | Some(resovedModuleName) =>
      [moduleName ++ ext] |> List.fold_left(concat, resovedModuleName)
    };
  print_endline(
    "resolveModule "
    ++ moduleName
    ++ " outputFileRelative "
    ++ outputFileRelative
    ++ " candidate "
    ++ candidate
    ++ " resolved "
    ++ resolved,
  );
  candidate;
};

let resolveSourceModule = (~outputFileRelative, ~resolver, moduleName) =>
  resolveModule(~outputFileRelative, ~resolver, ~ext=".bs", moduleName);

let resolveGeneratedModule = (~outputFileRelative, ~resolver, moduleName) =>
  resolveModule(~outputFileRelative, ~resolver, ~ext=".re", moduleName);