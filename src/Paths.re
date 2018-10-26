open GenTypeCommon;

let bsconfig = "bsconfig.json";

let rec findProjectRoot = (~dir) =>
  if (Sys.file_exists(Filename.concat(dir, bsconfig))) {
    dir;
  } else {
    let parent = dir |> Filename.dirname;
    if (parent == dir) {
      prerr_endline(
        "Error: cannot find project root containing " ++ bsconfig ++ ".",
      );
      assert(false);
    } else {
      findProjectRoot(~dir=parent);
    };
  };

let setProjectRoot = () => projectRoot := findProjectRoot(~dir=Sys.getcwd());

let concat = Filename.concat;

/*
 * Handle namespaces in cmt files.
 * E.g. src/Module-Project.cmt becomes src/Module
 */
let handleNamespace = cmt => {
  let cutAfterDash = s =>
    switch (String.index(s, '-')) {
    | n => String.sub(s, 0, n)
    | exception Not_found => s
    };
  let noDir = Filename.basename(cmt) == cmt;
  if (noDir) {
    cmt |> Filename.chop_extension |> cutAfterDash;
  } else {
    let dir = cmt |> Filename.dirname;
    let base =
      cmt |> Filename.basename |> Filename.chop_extension |> cutAfterDash;
    Filename.concat(dir, base);
  };
};

/* Get the output file to be written, relative to the project root. */
let getOutputFileRelative = (~config, cmt) =>
  (cmt |> handleNamespace) ++ EmitTyp.outputFileSuffix(~config);

/* Get the output file to be written, as an absolute path. */
let getOutputFile = (~config, cmt) =>
  Filename.concat(projectRoot^, getOutputFileRelative(~config, cmt));

let getModuleName = cmt =>
  cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe;

let getCmtFile = cmt => {
  let pathCmt = Filename.concat(Sys.getcwd(), cmt);
  let cmtFile =
    if (Filename.check_suffix(pathCmt, ".cmt")) {
      let pathCmti = Filename.chop_extension(pathCmt) ++ ".cmti";
      if (Sys.file_exists(pathCmti)) {
        pathCmti;
      } else if (Sys.file_exists(pathCmt)) {
        pathCmt;
      } else {
        "";
      };
    } else {
      "";
    };
  cmtFile;
};

let executable =
  Sys.executable_name |> Filename.is_relative ?
    concat(Unix.getcwd(), Sys.executable_name) : Sys.executable_name;

let getConfigFile = () => {
  let gentypeconfig = concat(projectRoot^, "gentypeconfig.json");
  gentypeconfig |> Sys.file_exists ? Some(gentypeconfig) : None;
};
let getBsConfigFile = () => {
  let bsconfig = concat(projectRoot^, "bsconfig.json");
  bsconfig |> Sys.file_exists ? Some(bsconfig) : None;
};

/* Find the relative path from /.../bs/lib
   e.g. /foo/bar/bs/lib/src/Hello.re --> src/Hello.re */
let relativePathFromBsLib = fileName =>
  if (Filename.is_relative(fileName)) {
    fileName;
  } else {
    let rec pathToList = path => {
      let isRoot = path |> Filename.basename == path;
      isRoot ?
        [path] :
        [path |> Filename.basename, ...path |> Filename.dirname |> pathToList];
    };
    let rec fromLibBs = (~acc, reversedList) =>
      switch (reversedList) {
      | ["bs", "lib", ..._] => acc
      | [dir, ...dirs] => fromLibBs(~acc=[dir, ...acc], dirs)
      | [] => [] /* not found */
      };
    fileName
    |> pathToList
    |> fromLibBs(~acc=[])
    |> (
      l =>
        switch (l) {
        | [] => fileName
        | [root, ...dirs] => dirs |> List.fold_left(concat, root)
        }
    );
  };

let getString = (s, json) =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt(s)) {
    | Some(Str({str, _})) => str
    | _ => ""
    }
  | _ => ""
  };

let getShims = json => {
  let shims = ref([]);
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt("shims")) {
    | Some(Ext_json_types.Obj({map: shimsMap, _})) =>
      shimsMap
      |> String_map.iter((fromModule, toModule) =>
           switch (toModule) {
           | Ext_json_types.Str({str, _}) =>
             shims := [(fromModule, str), ...shims^]
           | _ => ()
           }
         )
    | Some(Arr({content, _})) =>
      /* To be deprecated: array of strings */
      content
      |> Array.iter(x =>
           switch (x) {
           | Ext_json_types.Str({str, _}) =>
             let fromTo = Str.split(Str.regexp("="), str) |> Array.of_list;
             assert(Array.length(fromTo) === 2);
             shims := [(fromTo[0], fromTo[1]), ...shims^];
           | _ => ()
           }
         );
      ();
    | _ => ()
    }
  | _ => ()
  };
  shims^;
};

let readConfig = () => {
  setProjectRoot();
  let fromJson = json => {
    let languageString = json |> getString("language");
    let moduleString = json |> getString("module");
    let importPathString = json |> getString("importPath");
    let reasonReactPathString = json |> getString("reasonReactPath");
    let bsBlockPathString = json |> getString("bsBlockPath");
    let bsCurryPathString = json |> getString("bsCurryPath");
    let modulesMap =
      json
      |> getShims
      |> List.fold_left(
           (map, (fromModule, toModule)) => {
             let moduleName: ModuleName.t =
               fromModule |> ModuleName.fromStringUnsafe;
             let shimModuleName = toModule |> ModuleName.fromStringUnsafe;
             ModuleNameMap.add(moduleName, shimModuleName, map);
           },
           ModuleNameMap.empty,
         );
    if (Debug.config^) {
      logItem(
        "Config language:%s module:%s importPath:%s modulesMap:%d entries\n",
        languageString,
        moduleString,
        importPathString,
        modulesMap |> ModuleNameMap.cardinal,
      );
    };
    let language =
      switch (languageString) {
      | "typescript" => Typescript
      | "untyped" => Untyped
      | _ => Flow
      };
    let module_ =
      switch (moduleString) {
      | "commonjs" => CommonJS
      | "es6" => ES6
      | _ => Config_.default.module_
      };
    let importPath =
      switch (importPathString) {
      | "relative" => Relative
      | "node" => Node
      | _ => Config_.default.importPath
      };
    let reasonReactPath =
      switch (reasonReactPathString) {
      | "" => Config_.default.reasonReactPath
      | _ => reasonReactPathString
      };
    let bsBlockPath =
      switch (bsBlockPathString) {
      | "" => Config_.default.bsBlockPath
      | _ => bsBlockPathString
      };
    let bsCurryPath =
      switch (bsCurryPathString) {
      | "" => Config_.default.bsCurryPath
      | _ => bsCurryPathString
      };
    {
      ...Config_.default,
      language,
      module_,
      importPath,
      reasonReactPath,
      bsBlockPath,
      bsCurryPath,
      modulesMap,
    };
  };
  let fromBsConfig = json =>
    switch (json) {
    | Ext_json_types.Obj({map, _}) =>
      switch (map |> String_map.find_opt("gentypeconfig")) {
      | Some(jsonGenFlowConfig) => jsonGenFlowConfig |> fromJson
      | _ => Config_.default
      }
    | _ => Config_.default
    };
  switch (getConfigFile()) {
  | Some(configFile) =>
    try (configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
    | _ => Config_.default
    }
  | None =>
    switch (getBsConfigFile()) {
    | Some(bsConfigFile) =>
      try (bsConfigFile |> Ext_json_parse.parse_json_from_file |> fromBsConfig) {
      | _ => Config_.default
      }
    | None => Config_.default
    }
  };
};