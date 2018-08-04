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

let typedItemToCodeItems = (~config, ~moduleName, typedItem) => {
  let (listListDeps, listListItems) =
    switch (typedItem) {
    | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
      typeDeclarations
      |> List.map(CodeItem.fromTypeDecl(~config))
      |> List.split

    | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
      valueBindings
      |> List.map(CodeItem.fromValueBinding(~config, ~moduleName))
      |> List.split

    | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
      /* external declaration */
      valueDescription |> CodeItem.fromValueDescription(~config)

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
            nextTypedItem |> typedItemToCodeItems(~config, ~moduleName);
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

let emitCodeItems =
    (
      ~config,
      ~outputFile,
      ~outputFileRelative,
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
      codeItems
      |> EmitJs.emitCodeItems(~config, ~outputFileRelative, ~resolver);
    let fileContents =
      signFile(EmitTyp.fileHeader(~config) ++ "\n" ++ codeText);

    GeneratedFiles.writeFileIfRequired(~fileName=outputFile, ~fileContents);
  };

let processCmtFile = (~signFile, ~config, cmt) => {
  let cmtFile = Filename.concat(Sys.getcwd(), cmt);
  if (Sys.file_exists(cmtFile)) {
    GenIdent.resetPerFile();
    let inputCMT = Cmt_format.read_cmt(cmtFile);
    let outputFile = cmt |> Paths.getOutputFile(~config);
    let outputFileRelative = cmt |> Paths.getOutputFileRelative(~config);
    let moduleName = cmt |> Paths.getModuleName;
    let resolver =
      ModuleResolver.createResolver(~extensions=[".re", ".shim.js"]);
    inputCMT
    |> cmtToCodeItems(~config, ~moduleName, ~outputFileRelative, ~resolver)
    |> emitCodeItems(
         ~config,
         ~outputFile,
         ~outputFileRelative,
         ~signFile,
         ~resolver,
       );
  };
};