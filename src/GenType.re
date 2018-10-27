/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenTypeCommon;

let version = Version.version;

let signFile = s => s;

let cli = () => {
  let setCmtAdd = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 2);
    let cmt: string = splitColon[0];
    let mlast: string = splitColon[1];
    let config = Paths.readConfig();
    if (Debug.basic^) {
      logItem("Add %s  %s\n", cmt, mlast);
    };
    cmt |> GenTypeMain.processCmtFile(~signFile, ~config);
    exit(0);
  };
  let setCmtRm = s => {
    let splitColon = Str.split(Str.regexp(":"), s) |> Array.of_list;
    assert(Array.length(splitColon) === 1);
    let cmtAbsolutePath: string = splitColon[0];
    /* somehow the CMT hook is passing an absolute path here */
    let cmt = cmtAbsolutePath |> Paths.relativePathFromBsLib;
    let config = Paths.readConfig();
    let outputFile = cmt |> Paths.getOutputFile(~config);
    if (Debug.basic^) {
      logItem("Remove %s\n", cmt);
    };
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
    exit(0);
  };
  let usage = "genType version " ++ version;
  let version = () => {
    print_endline(usage);
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
    ("-version", Arg.Unit(version), "show version information"),
    ("--version", Arg.Unit(version), "show version information"),
  ];
  Arg.parse(speclist, print_endline, usage);
};

cli();