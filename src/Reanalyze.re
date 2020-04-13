let version = Version.version;

type cliCommand =
  | DCE(option(string))
  | NoOp
  | Termination(option(string));

let cli = () => {
  let cliCommand = ref(NoOp);
  let usage = "reanalyze version " ++ version;
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
  and setDCE = cmtRoot => {
    DCE(cmtRoot) |> setCliCommand;
  }
  and setTermination = cmtRoot => {
    Termination(cmtRoot) |> setCliCommand;
  }
  and speclist = [
    ("-dce", Arg.Unit(() => setDCE(None)), "experimental DCE"),
    (
      "-dce-cmt",
      Arg.String(s => setDCE(Some(s))),
      "root_path experimental DCE for all the .cmt files under the root path",
    ),
    (
      "-termination",
      Arg.Unit(() => setTermination(None)),
      "experimental termination",
    ),
    (
      "-termination-cmt",
      Arg.String(s => setTermination(Some(s))),
      "root_path experimental termination for all the .cmt files under the root path",
    ),
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

  let executeCliCommand = cliCommand =>
    switch (cliCommand) {
    | NoOp => printUsageAndExit()
    | DCE(cmtRoot) => DeadCode.runAnalysis(~cmtRoot)
    | Termination(cmtRoot) => DeadCode.runTerminationAnalysis(~cmtRoot)
    };

  Arg.parse(speclist, print_endline, usage);

  executeCliCommand(cliCommand^);
};

cli();
