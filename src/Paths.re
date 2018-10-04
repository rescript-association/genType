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
let getOutputFileRelative = (~language, cmt) =>
  (cmt |> handleNamespace) ++ EmitTyp.outputFileSuffix(~language);

/* Get the output file to be written, as an absolute path. */
let getOutputFile = (~language, cmt) =>
  Filename.concat(projectRoot^, getOutputFileRelative(~language, cmt));

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
  let genflowconfig = concat(projectRoot^, "genflowconfig.json");
  gentypeconfig |> Sys.file_exists ?
    Some(gentypeconfig) :
    genflowconfig |> Sys.file_exists ? Some(genflowconfig) : None;
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

let getLanguage = json =>
  switch (json) {
  | Ext_json_types.Obj({map, _}) =>
    switch (map |> String_map.find_opt("language")) {
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
    | Some(Arr({content, _})) =>
      content
      |> Array.iter(x =>
           switch (x) {
           | Ext_json_types.Str({str, _}) => shims := [str, ...shims^]
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

let emptyConfig = {
  language: Flow,
  modulesMap: ModuleNameMap.empty,
  module_: ES6,
};
let readConfig = () => {
  let fromJson = json => {
    let languageString = json |> getLanguage;
    let modulesMap =
      json
      |> getShims
      |> List.fold_left(
           (map, nextPairStr) =>
             if (nextPairStr != "") {
               let fromTo =
                 Str.split(Str.regexp("="), nextPairStr) |> Array.of_list;
               assert(Array.length(fromTo) === 2);
               let moduleName: ModuleName.t =
                 fromTo[0] |> ModuleName.fromStringUnsafe;
               let shimModuleName = fromTo[1] |> ModuleName.fromStringUnsafe;
               ModuleNameMap.add(moduleName, shimModuleName, map);
             } else {
               map;
             },
           ModuleNameMap.empty,
         );
    if (Debug.config) {
      logItem(
        "Config language:%s modulesMap:%d entries\n",
        languageString,
        modulesMap |> ModuleNameMap.cardinal,
      );
    };
    let language =
      switch (languageString) {
      | "typescript" => Typescript
      | "untyped" => Untyped
      | _ => Flow
      };
    {...emptyConfig, language, modulesMap};
  };

  switch (getConfigFile()) {
  | None => emptyConfig
  | Some(configFile) =>
    try (configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
    | _ => emptyConfig
    }
  };
};