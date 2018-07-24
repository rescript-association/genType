/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenFlowCommon;

let nullyToOptionalConverter = nullyValue => {};

let pp = Printf.fprintf;

let typedItemToCodeItems = (~inputModuleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations)} =>
      typeDeclarations
      |> List.map(CodeItem.fromTypeDecl(~inputModuleName))
      |> List.split
    | {Typedtree.str_desc: Tstr_value(loc, valueBindings)} =>
      valueBindings
      |> List.map(CodeItem.fromValueBinding(~inputModuleName))
      |> List.split
    | _ => ([], [])
    /* TODO: Support mapping of variant type definitions. */
    };
  (List.concat(listListDeps), List.concat(listListItems));
};

let cmtToCodeItems =
    (~modulesMap, ~globalModuleName, inputCMT): list(CodeItem.t) => {
  let {Cmt_format.cmt_annots} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    let typedItems = structure.Typedtree.str_items;
    let (deps, codeItems) =
      List.fold_left(
        ((curDeps, curParseItems), nextTypedItem) => {
          let (nextDeps, nextCodeItems) =
            nextTypedItem
            |> typedItemToCodeItems(~inputModuleName=globalModuleName);
          (nextDeps @ curDeps, nextCodeItems @ curParseItems);
        },
        ([], []),
        typedItems,
      );
    let imports = CodeItem.fromDependencies(modulesMap, deps);
    List.append(imports, codeItems);
  | _ => []
  };
};

let log = Printf.printf;
let logItem = x => {
  log("  ");
  log(x);
};

module GeneratedReFiles = {
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

  let writeFileIfRequired = (~fileName, ~fileContents, ~writeFile) =>
    if (Sys.file_exists(fileName)) {
      let oldContents = readFile(fileName);
      let identical = oldContents == fileContents;
      if (identical) {
        fileName |> logFileAction(Identical);
      };
      if (!identical) {
        fileName |> logFileAction(Replace);
        writeFile(fileName, fileContents);
      };
    } else {
      fileName |> logFileAction(Write);
      writeFile(fileName, fileContents);
    };
};

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath);
  output_string(outFile, contents);
  close_out(outFile);
};

let emitCodeItems = (~outputFile, ~fileHeader, ~signFile, codeItems) =>
  switch (codeItems) {
  | [_, ..._] =>
    let codeText = codeItems |> EmitJs.emitCodeItems;
    let fileContents = signFile(fileHeader ++ "\n" ++ codeText);

    GeneratedReFiles.writeFileIfRequired(
      ~fileName=outputFile,
      ~fileContents,
      ~writeFile,
    );

  | [] => outputFile |> GeneratedReFiles.logFileAction(NoMatch)
  };

let processCmtFile =
    (~outputFile, ~fileHeader, ~signFile, ~modulesMap, cmtFile) => {
  GenIdent.resetPerFile();
  let inputCMT = Cmt_format.read_cmt(cmtFile);
  let globalModuleName = Filename.chop_extension(Filename.basename(cmtFile));
  inputCMT
  |> cmtToCodeItems(~modulesMap, ~globalModuleName)
  |> emitCodeItems(~outputFile, ~fileHeader, ~signFile);
};