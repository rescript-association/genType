/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

open GenTypeCommon;

let version = Version.version;

let signFile = s => s;

type cliCommand =
  | Add(string)
  | Clean
  | NoOp
  | Rm(list(string));

let cli = () => {
  let bsVersion = ref(None);
  let cliCommand = ref(NoOp);
  let setBsVersion = s => {
    bsVersion := Some(s);
  };
  let usage = "genType version " ++ version;
  let versionAndExit = () => {
    print_endline(usage);
    exit(0);
  };
  let rec printUsageAndExit = () => {
    Arg.usage(speclist, usage);
    exit(0);
  }
  and setCliCommand = command => {
    if (cliCommand^ != NoOp) {
      printUsageAndExit();
    };
    cliCommand := command;
  }
  and setAdd = s => {
    Add(s) |> setCliCommand;
  }
  and setRm = s => {
    Rm(s) |> setCliCommand;
  }
  and setClean = () => {
    Clean |> setCliCommand;
  }
  and speclist = [
    (
      "-bs-version",
      Arg.String(setBsVersion),
      "set the bucklescript version",
    ),
    ("-clean", Arg.Unit(setClean), "clean all the generated files"),
    ("-cmt-add", Arg.String(setAdd), "compile a .cmt[i] file"),
    ("-cmt-rm", Arg.String(s => setRm([s])), "remove one or more .cmt[i] files"),
    (
      "-version",
      Arg.Unit(versionAndExit),
      "show version information and exit",
    ),
    (
      "--version",
      Arg.Unit(versionAndExit),
      "show version information and exit",
    ),
  ];

  let executeCliCommand = (~bsVersion, cliCommand) =>
    switch (cliCommand) {
    | Add(s) =>
      Log_.Color.forceColor := true;
      let splitColon = Str.split(Str.regexp(":"), s);
      let (cmt, mlast) =
        switch (splitColon) {
        | [cmt, ...rest] =>
          let mlast = rest |> String.concat("");
          (cmt, mlast);
        | _ => assert(false)
        };
      let config =
        Paths.readConfig(~bsVersion, ~namespace=cmt |> Paths.findNameSpace);
      if (Debug.basic^) {
        Log_.item("Add %s  %s\n", cmt, mlast);
      };
      cmt |> GenTypeMain.processCmtFile(~signFile, ~config);
      exit(0);

    | Clean =>
      let config = Paths.readConfig(~bsVersion, ~namespace=None);
      let sourceDirs =
        ModuleResolver.readSourceDirs(~configSources=config.sources);
      if (Debug.basic^) {
        Log_.item("Clean %d dirs\n", sourceDirs.dirs |> List.length);
      };
      let count = ref(0);
      sourceDirs.dirs
      |> List.iter(dir => {
           let files = Sys.readdir(dir);
           files
           |> Array.iter(file =>
                if (Filename.check_suffix(file, ".re")
                    || Filename.check_suffix(file, ".res")) {
                  let extension = EmitType.outputFileSuffix(~config);
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
        Log_.item("Cleaned %d files\n", count^);
      };
      exit(0);

    | NoOp => printUsageAndExit()

    | Rm(l) =>
      let removeOne = s => {
        let cmtAbsolutePath = s;
        /* somehow the CMT hook is passing an absolute path here */
        let cmt = cmtAbsolutePath |> Paths.relativePathFromBsLib;
        let config =
          Paths.readConfig(~bsVersion, ~namespace=cmt |> Paths.findNameSpace);
        let outputFile = cmt |> Paths.getOutputFile(~config);
        if (Debug.basic^) {
          Log_.item("Remove %s\n", cmt);
        };
        if (Sys.file_exists(outputFile)) {
          Unix.unlink(outputFile);
        };
      };
      l |> List.rev |> List.iter(removeOne);
      exit(0);
    };

  let anonArg = s => {
    switch (cliCommand^) {
    | Rm(l) => cliCommand := Rm([s, ...l])
    | _ => print_endline(s)
    };
  };

  Arg.parse(speclist, anonArg, usage);

  executeCliCommand(~bsVersion=bsVersion^, cliCommand^);
};

cli();
