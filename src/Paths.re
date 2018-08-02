open GenFlowCommon;

let outputFileSuffix = ".re.js";

let projectRoot = ref(Sys.getcwd());

let setProjectRoot = s =>
  projectRoot :=
    Filename.is_relative(s) ? Filename.concat(Unix.getcwd(), s) : s;

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
let getOutputFileRelative = cmt =>
  (cmt |> handleNamespace) ++ outputFileSuffix;

/* Get the output file to be written, as an absolute path. */
let getOutputFile = cmt =>
  Filename.concat(projectRoot^, getOutputFileRelative(cmt));

let getModuleName = cmt =>
  cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe;

let executable =
  Sys.executable_name |> Filename.is_relative ?
    concat(Unix.getcwd(), Sys.executable_name) : Sys.executable_name;

let getConfigFile = () => {
  let fname = concat(projectRoot^, "genflowconfig.json");
  fname |> Sys.file_exists ? Some(fname) : None;
};

/* Find the relative path from /.../bs/lib
   e.g. /foo/bar/bs/lib/src/Hello.re --> src/Hello.re */
let relativePathFromBsLib = fileName =>
  if (Filename.is_relative(fileName)) {
    fileName;
  } else {
    let rec pathToList = path => {
      let isRoot = path |> Filename.basename == path;
      isRoot ? [path] : [path |> Filename.basename, ...path |> Filename.dirname |> pathToList];
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

let emptyConfig = {language: "", modulesMap: ModuleNameMap.empty};
let readConfig = () => {
  let fromJson = json => {
    let language = json |> getLanguage;
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
        language,
        modulesMap |> ModuleNameMap.cardinal,
      );
    };
    {language, modulesMap};
  };

  switch (getConfigFile()) {
  | None => emptyConfig
  | Some(configFile) =>
    try (configFile |> Ext_json_parse.parse_json_from_file |> fromJson) {
    | _ => emptyConfig
    }
  };
};