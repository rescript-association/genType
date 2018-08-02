/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenFlowCommon;

let getPriority = x =>
  switch (x) {
  | CodeItem.ImportType(_)
  | ExternalReactClass(_) => "2low"
  | ValueBinding(_)
  | ConstructorBinding(_)
  | ComponentBinding(_)
  | ExportType(_)
  | ExportUnionType(_) => "1med"
  };

let sortcodeItemsByPriority = codeItems => {
  module M = StringMap;
  let map =
    codeItems
    |> List.fold_left(
         (map, codeItem) => {
           let priority = codeItem |> getPriority;
           let items =
             try (map |> StringMap.find(priority)) {
             | Not_found => []
             };
           map |> StringMap.add(priority, [codeItem, ...items]);
         },
         StringMap.empty,
       );
  let sortedCodeItems = ref([]);
  map
  |> StringMap.iter((priority, codeItemsAtPriority) =>
       codeItemsAtPriority
       |> List.iter(codeItem =>
            sortedCodeItems := [codeItem, ...sortedCodeItems^]
          )
     );
  sortedCodeItems^;
};

let typedItemToCodeItems = (~moduleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
      typeDeclarations |> List.map(CodeItem.fromTypeDecl) |> List.split

    | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
      valueBindings
      |> List.map(CodeItem.fromValueBinding(~moduleName))
      |> List.split

    | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
      /* external declaration */
      valueDescription |> CodeItem.fromValueDescription

    | _ => ([], [])
    /* TODO: Support mapping of variant type definitions. */
    };
  (List.concat(listListDeps), List.concat(listListItems));
};

let cmtToCodeItems =
    (~config, ~moduleName, ~outputFileRelative, ~resolver, inputCMT)
    : list(CodeItem.t) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  switch (cmt_annots) {
  | Implementation(structure) =>
    let typedItems = structure.Typedtree.str_items;
    let (deps, revCodeItems) =
      List.fold_left(
        ((curDeps, curParseItems), nextTypedItem) => {
          let (nextDeps, nextCodeItems) =
            nextTypedItem |> typedItemToCodeItems(~moduleName);
          (
            List.rev_append(nextDeps, curDeps),
            List.rev_append(nextCodeItems, curParseItems),
          );
        },
        ([], []),
        typedItems,
      );
    let codeItems = revCodeItems |> List.rev;
    let imports =
      CodeItem.fromDependencies(
        ~outputFileRelative,
        ~resolver,
        ~config,
        deps,
      );
    List.append(imports, codeItems |> sortcodeItemsByPriority);
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
  if (codeItems == []) {
    outputFile |> GeneratedFiles.logFileAction(NoMatch);
    if (Sys.file_exists(outputFile)) {
      Unix.unlink(outputFile);
    };
  } else {
    let codeText =
      codeItems |> EmitJs.emitCodeItems(~outputFileRelative, ~resolver);
    let fileContents = signFile(fileHeader ++ "\n" ++ codeText);

    GeneratedFiles.writeFileIfRequired(~fileName=outputFile, ~fileContents);
  };

let processCmtFile = (~fileHeader, ~signFile, ~config, cmt) => {
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
    |> cmtToCodeItems(~config, ~moduleName, ~outputFileRelative, ~resolver)
    |> emitCodeItems(
         ~outputFile,
         ~outputFileRelative,
         ~fileHeader,
         ~signFile,
         ~resolver,
       );
  };
};