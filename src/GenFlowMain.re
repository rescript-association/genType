/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenFlowCommon;

let typedItemToCodeItems = (~moduleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
      typeDeclarations |> List.map(CodeItem.fromTypeDecl) |> List.split
    | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
      valueBindings
      |> List.map(CodeItem.fromValueBinding(~moduleName))
      |> List.split
    | _ => ([], [])
    /* TODO: Support mapping of variant type definitions. */
    };
  (List.concat(listListDeps), List.concat(listListItems));
};

let cmtToCodeItems =
    (~modulesMap, ~moduleName, ~outputFileRelative, ~resolver, inputCMT)
    : list(CodeItem.t) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    let typedItems = structure.Typedtree.str_items;
    let (deps, codeItems) =
      List.fold_left(
        ((curDeps, curParseItems), nextTypedItem) => {
          let (nextDeps, nextCodeItems) =
            nextTypedItem |> typedItemToCodeItems(~moduleName);
          (nextDeps @ curDeps, nextCodeItems @ curParseItems);
        },
        ([], []),
        typedItems,
      );
    let imports =
      CodeItem.fromDependencies(
        ~outputFileRelative,
        ~resolver,
        ~modulesMap,
        deps,
      );
    List.append(imports, codeItems);
  | _ => []
  };
};

module GeneratedFiles = {
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

  let writeFileIfRequired = (~fileName, ~fileContents, ~writeFile) =>
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
};

let writeFile = (filePath: string, contents: string) => {
  let outFile = open_out(filePath);
  output_string(outFile, contents);
  close_out(outFile);
};

let emitCodeItems =
    (
      ~outputFile,
      ~outputFileRelative,
      ~fileHeader,
      ~signFile,
      ~resolver,
      codeItems,
    ) =>
  switch (codeItems) {
  | [_, ..._] =>
    let codeText =
      codeItems |> EmitJs.emitCodeItems(~outputFileRelative, ~resolver);
    let fileContents = signFile(fileHeader ++ "\n" ++ codeText);

    GeneratedFiles.writeFileIfRequired(
      ~fileName=outputFile,
      ~fileContents,
      ~writeFile,
    );

  | [] =>
    outputFile |> GeneratedFiles.logFileAction(NoMatch);
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
  };

let processCmtFile = (~fileHeader, ~signFile, ~modulesMap, cmt) => {
  let cmtFile = Filename.concat(Sys.getcwd(), cmt);
  if (Sys.file_exists(cmtFile)) {
    GenIdent.resetPerFile();
    let inputCMT = Cmt_format.read_cmt(cmtFile);
    let outputFile = cmt |> Paths.getOutputFile;
    let outputFileRelative = cmt |> Paths.getOutputFileRelative;
    let moduleName = cmt |> Paths.getModuleName;
    let resolver =
      ModuleResolver.createResolver(~extensions=[".re", ".shim.js"]);
    inputCMT
    |> cmtToCodeItems(
         ~modulesMap,
         ~moduleName,
         ~outputFileRelative,
         ~resolver,
       )
    |> emitCodeItems(
         ~outputFile,
         ~outputFileRelative,
         ~fileHeader,
         ~signFile,
         ~resolver,
       );
  };
};