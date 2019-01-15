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
    let config = Paths.readConfig(~namespace=cmt |> Paths.findNameSpace);
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
    let config = Paths.readConfig(~namespace=cmt |> Paths.findNameSpace);
    let outputFile = cmt |> Paths.getOutputFile(~config);
    if (Debug.basic^) {
      logItem("Remove %s\n", cmt);
    };
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
    exit(0);
  };
  let clean = () => {
    let config = Paths.readConfig(~namespace=None);
    let dirs = ModuleResolver.readSourceDirs();
    if (Debug.basic^) {
      logItem("Clean %d dirs\n", dirs |> List.length);
    };
    let count = ref(0);
    dirs
    |> List.iter(dir => {
         let files = Sys.readdir(dir);
         files
         |> Array.iter(file =>
              if (Filename.check_suffix(file, ".re")) {
                let extension = EmitTyp.outputFileSuffix(~config);
                let generated =
                  Filename.concat(
                    dir,
                    (file |> Filename.chop_extension) ++ extension,
                  );
                if (Sys.file_exists(generated)) {
                  Unix.unlink(generated);
                  incr(count);
                };
              }
            );
       });
    if (Debug.basic^) {
      logItem("Cleaned %d files\n", count^);
    };
    exit(0);
  };
  let usage = "genType version " ++ version;
  let version = () => {
    print_endline(usage);
    exit(0);
  };
  let speclist = [
    ("-clean", Arg.Unit(clean), "clean all the generated files"),
    ("-cmt-add", Arg.String(setCmtAdd), "compile a .cmt[i] file"),
    ("-cmt-rm", Arg.String(setCmtRm), "remove a .cmt[i] file"),
    ("-version", Arg.Unit(version), "show version information"),
    ("--version", Arg.Unit(version), "show version information"),
  ];
  Arg.parse(speclist, print_endline, usage);
};

cli();