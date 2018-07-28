/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenFlowCommon;

let getShims = configFile => {
  let parseJson = json => {
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
  try (configFile |> Ext_json_parse.parse_json_from_file |> parseJson) {
  | _ => []
  };
};

let createModulesMap = configFileOpt =>
  switch (configFileOpt) {
  | None => ModuleNameMap.empty
  | Some(configFile) =>
    configFile
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
       )
  };

let fileHeader = "/* @flow strict */\n";

let signFile = s => s;

let cli = () => {
  let setProjectRoot = Paths.setProjectRoot;
  let setCmtAdd = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 2);
    let cmt: string = splitColon[0];
    let mlast: string = splitColon[1];
    logItem("Add %s  %s\n", cmt, mlast);
    cmt
    |> GenFlowMain.processCmtFile(
         ~fileHeader,
         ~signFile,
         ~modulesMap=Paths.getConfigFile() |> createModulesMap,
       );
    exit(0);
  };
  let setCmtRm = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 1);
    let cmtAbsolutePath: string = splitColon[0];
    /* somehow the CMT hook is passing an absolute path here */
    let cmt = cmtAbsolutePath |> Paths.relativePathFromBsLib;
    let outputFile = cmt |> Paths.getOutputFile;
    logItem("Remove %s\n", cmt);
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
    exit(0);
  };
  let speclist = [
    (
      "--setProjectRoot",
      Arg.String(setProjectRoot),
      "set the root of the bucklescript project",
    ),
    ("-cmt-add", Arg.String(setCmtAdd), "compile a .cmt[i] file"),
    ("-cmt-rm", Arg.String(setCmtRm), "remove a .cmt[i] file"),
  ];
  let usage = "genFlow";
  Arg.parse(speclist, print_endline, usage);
};

cli();