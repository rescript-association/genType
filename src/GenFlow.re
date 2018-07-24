/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenFlowCommon;

module Paths = {
  open Filename;
  let executable =
    Sys.executable_name |> is_relative ?
      concat(Unix.getcwd(), Sys.executable_name) : Sys.executable_name;
  let projectRoot = ref(Sys.getcwd());
  let outputDir = () =>
    Filename.(
      List.fold_left(concat, projectRoot^, ["src", "__generated_flow__"])
    );

  let defaultModulesMap = () => concat(projectRoot^, "modulesMap.txt");
  let absoluteFromProject = filePath =>
    Filename.(
      filePath |> is_relative ? concat(projectRoot^, filePath) : filePath
    );

  let concat = concat;
};

let createModulesMap = modulesMapFile => {
  let s = GenFlowMain.GeneratedReFiles.readFile(modulesMapFile);
  Str.split(Str.regexp("\n"), s)
  |> List.fold_left(
       (map, nextPairStr) =>
         if (nextPairStr != "") {
           let fromTo =
             Str.split(Str.regexp("="), nextPairStr) |> Array.of_list;
           assert(Array.length(fromTo) === 2);
           let k: string = fromTo[0];
           let v: string = fromTo[1];
           StringMap.add(k, v, map);
         } else {
           map;
         },
       StringMap.empty,
     );
};

let findCmtFiles = (): list(string) => {
  open Paths;
  let src = ["lib", "bs", "src"] |> List.fold_left(concat, projectRoot^);
  let cmtFiles =
    src
    |> Sys.readdir
    |> Array.to_list
    |> List.filter(Filename.check_suffix(_, ".cmt"));
  cmtFiles |> List.map(concat(src));
};

let fileHeader = "/* @flow strict */\n";

let signFile = s => s;

let buildSourceFiles = () => ();
/* TODO */

let buildGeneratedFiles = () => ();
/* TODO */

let modulesMap = {
  let default =
    Sys.file_exists(Paths.defaultModulesMap()) ?
      Some(Paths.defaultModulesMap()) : None;
  ref(default);
};

let cli = () => {
  let setProjectRoot = s =>
    Paths.projectRoot :=
      Filename.is_relative(s) ? Filename.concat(Unix.getcwd(), s) : s;
  let setModulesMap = s => modulesMap := Some(s |> Paths.absoluteFromProject);
  let getModulesMap = () =>
    switch (modulesMap^) {
    | None => Paths.defaultModulesMap() |> createModulesMap
    | Some(path) => path |> createModulesMap
    };
  let setCmtAdd = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 2);
    let cmt: string = splitColon[0];
    let mlast: string = splitColon[1];
    let cmtFile = Filename.concat(Sys.getcwd(), cmt);
    let cmtExists = Sys.file_exists(cmtFile);
    let shouldProcess = cmtExists && Filename.check_suffix(cmtFile, ".cmt");
    if (shouldProcess) {
      print_endline("  Add " ++ cmt ++ "  " ++ mlast);
      cmtFile
      |> GenFlowMain.processCmtFile(
           ~outputDir=Paths.outputDir(),
           ~fileHeader,
           ~signFile,
           ~modulesMap=getModulesMap(),
         );
    };
    exit(0);
  };
  let setCmtRm = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 1);
    let cmt: string = splitColon[0];
    let globalModuleName = Filename.chop_extension(Filename.basename(cmt));
    let re =
      Paths.(
        concat(
          outputDir(),
          outputReasonModuleName(globalModuleName) ++ suffix,
        )
      );
    print_endline("  Remove " ++ cmt);
    if (Sys.file_exists(re)) {
      Unix.unlink(re);
    };
    exit(0);
  };
  let speclist = [
    (
      "--setProjectRoot",
      Arg.String(setProjectRoot),
      "set the root of the bucklescript project",
    ),
    (
      "--modulesMap",
      Arg.String(setModulesMap),
      "Specify map file to override the JS module resolution for dependencies that would"
      ++ " normally be generated by genFlow but are not available for whatever reason."
      ++ " Example(--modulesMap map.txt) where each line is of the form 'ModuleFlow.bs=SomeShim'. "
      ++ "E.g. 'ReasonReactFlow.bs=ReasonReactShim'.",
    ),
    ("-cmt-add", Arg.String(setCmtAdd), "compile a .cmt file"),
    ("-cmt-rm", Arg.String(setCmtRm), "remove a .cmt file"),
  ];
  let usage = "genFlow";
  Arg.parse(speclist, print_endline, usage);
};

cli();