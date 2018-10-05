/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenTypeCommon;

let signFile = s => s;

let cli = () => {
  let setProjectRoot = Paths.setProjectRoot;
  let setCmtAdd = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 2);
    let cmt: string = splitColon[0];
    let mlast: string = splitColon[1];
    if (Debug.basic) {
      logItem("Add %s  %s\n", cmt, mlast);
    };
    setProjectRoot();
    let config = Paths.readConfig();
    cmt |> GenTypeMain.processCmtFile(~signFile, ~config);
    exit(0);
  };
  let setCmtRm = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 1);
    let cmtAbsolutePath: string = splitColon[0];
    /* somehow the CMT hook is passing an absolute path here */
    let cmt = cmtAbsolutePath |> Paths.relativePathFromBsLib;
    setProjectRoot();
    let config = Paths.readConfig();
    let outputFile = cmt |> Paths.getOutputFile(~language=config.language);
    if (Debug.basic) {
      logItem("Remove %s\n", cmt);
    };
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
    exit(0);
  };
  let speclist = [
    (
      "--setProjectRoot",
      Arg.String(_ => ()),
      "This option is deprecated and is ignored (project root found automatically).",
    ),
    ("-cmt-add", Arg.String(setCmtAdd), "compile a .cmt[i] file"),
    ("-cmt-rm", Arg.String(setCmtRm), "remove a .cmt[i] file"),
  ];
  let usage = "genType";
  Arg.parse(speclist, print_endline, usage);
};

cli();