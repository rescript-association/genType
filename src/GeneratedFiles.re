open GenFlowCommon;

type fileAction =
  | NoMatch /* No @genFlow annotation found. */
  | Replace /* Replace existing file on disk with new contents. */
  | Identical /* File already on disk with identical contents. Skip. */
  | Write; /* File not present on disk. */

let logFileAction = (fileAction, fileName) =>
  logItem(
    "%s  %s\n",
    switch (fileAction) {
    | NoMatch => "NoMatch"
    | Replace => "Replace"
    | Identical => "Identical"
    | Write => "Write"
    },
    fileName,
  );

let readLines = (file: string): list(string) => {
  let lines = ref([]);
  let chan = open_in(file);
  let finished_lines =
    try (
      {
        while (true) {
          lines := [input_line(chan), ...lines^];
        };
        [];
      }
    ) {
    | End_of_file =>
      close_in(chan);
      lines^ |> List.rev;
    };
  finished_lines;
};

let readFile = (file: string): string =>
  String.concat("\n", readLines(file));

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath);
  output_string(outFile, contents);
  close_out(outFile);
};

let writeFileIfRequired = (~fileName, ~fileContents) =>
  if (Sys.file_exists(fileName)) {
    let oldContents = readFile(fileName);
    let identical = oldContents == fileContents;
    if (identical) {
      fileName |> logFileAction(Identical);
    } else {
      fileName |> logFileAction(Replace);
      writeFile(fileName, fileContents);
    };
  } else {
    fileName |> logFileAction(Write);
    writeFile(fileName, fileContents);
  };