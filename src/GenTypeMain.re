/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenTypeCommon;

let getPriority = x =>
  switch (x) {
  | CodeItem.ImportType(_)
  | WrapJsComponent(_)
  | WrapJsValue(_) => "2low"
  | ExportType(_)
  | ExportVariantType(_)
  | WrapReasonComponent(_)
  | WrapReasonValue(_)
  | WrapVariantLeaf(_) => "1med"
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
  |> StringMap.iter((_priority, codeItemsAtPriority) =>
       codeItemsAtPriority
       |> List.iter(codeItem =>
            sortedCodeItems := [codeItem, ...sortedCodeItems^]
          )
     );
  sortedCodeItems^;
};

let cmtHasGenTypeAnnotations = inputCMT =>
  switch (inputCMT.Cmt_format.cmt_annots) {
  | Implementation(structure) =>
    structure |> CodeItem.structureHasGenTypeAnnotation
  | Interface(signature) =>
    signature |> CodeItem.signatureHasGenTypeAnnotation
  | _ => false
  };

let typeDeclarationsOfStructItem = structItem =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} => typeDeclarations
  | _ => []
  };

let typeDeclarationsOfSignature = signatureItem =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} => typeDeclarations
  | _ => []
  };

let inputCmtToTypeDeclarations = (~language, inputCMT): list(CodeItem.t) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let typeEnv = TypeEnv.root();
  let typeDeclarations =
    (
      switch (cmt_annots) {
      | Implementation(structure) =>
        structure.Typedtree.str_items
        |> List.map(typeDeclarationsOfStructItem)
      | Interface(signature) =>
        signature.Typedtree.sig_items |> List.map(typeDeclarationsOfSignature)
      | _ => []
      }
    )
    |> List.concat;
  typeDeclarations
  |> List.map(typeDeclaration =>
       (
         typeDeclaration
         |> CodeItem.translateTypeDeclaration(~language, ~typeEnv)
       ).
         codeItems
     )
  |> List.concat;
};

let cmtToCodeItems =
    (~config, ~moduleName, ~outputFileRelative, ~resolver, inputCMT)
    : list(CodeItem.t) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let propsTypeGen = GenIdent.createPropsTypeGen();
  let typeEnv = TypeEnv.root();
  let translationUnits =
    switch (cmt_annots) {
    | Implementation(structure) =>
      structure
      |> CodeItem.translateStructure(
           ~config,
           ~propsTypeGen,
           ~moduleName,
           ~typeEnv,
         )
    | Interface(signature) =>
      signature
      |> CodeItem.translateSignature(
           ~config,
           ~propsTypeGen,
           ~moduleName,
           ~typeEnv,
         )
    | _ => []
    };
  let translationUnit = translationUnits |> CodeItem.combineTranslations;
  let imports =
    translationUnit.dependencies
    |> CodeItem.translateDependencies(~config, ~outputFileRelative, ~resolver);
  let sortedCodeItems = translationUnit.codeItems |> sortcodeItemsByPriority;
  imports @ sortedCodeItems;
};

let emitCodeItems =
    (
      ~config,
      ~outputFile,
      ~outputFileRelative,
      ~signFile,
      ~resolver,
      codeItems,
    ) => {
  let language = config.language;
  let codeText =
    codeItems
    |> EmitJs.emitCodeItemsAsString(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~inputCmtToTypeDeclarations,
       );
  let fileContents =
    signFile(EmitTyp.fileHeader(~language) ++ "\n" ++ codeText ++ "\n");

  GeneratedFiles.writeFileIfRequired(~fileName=outputFile, ~fileContents);
};

let processCmtFile = (~signFile, ~config, cmt) => {
  let cmtFile = cmt |> Paths.getCmtFile;
  if (cmtFile != "") {
    let inputCMT = Cmt_format.read_cmt(cmtFile);
    let outputFile = cmt |> Paths.getOutputFile(~language=config.language);
    let outputFileRelative =
      cmt |> Paths.getOutputFileRelative(~language=config.language);
    let moduleName = cmt |> Paths.getModuleName;
    let resolver =
      ModuleResolver.createResolver(
        ~extensions=[
          ".re",
          EmitTyp.shimExtension(~language=config.language),
        ],
        ~excludeFile=fname =>
        fname == "React.re" || fname == "ReasonReact.re"
      );
    if (inputCMT |> cmtHasGenTypeAnnotations) {
      inputCMT
      |> cmtToCodeItems(~config, ~moduleName, ~outputFileRelative, ~resolver)
      |> emitCodeItems(
           ~config,
           ~outputFile,
           ~outputFileRelative,
           ~signFile,
           ~resolver,
         );
    } else {
      outputFile |> GeneratedFiles.logFileAction(NoMatch);
      if (Sys.file_exists(outputFile)) {
        Unix.unlink(outputFile);
      };
    };
  };
};