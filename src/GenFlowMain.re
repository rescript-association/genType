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
  | ExportVariantType(_) => "1med"
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

let hasGenTypeAnnotation = attributes =>
  CodeItem.getGenTypeKind(attributes) != NoGenType;

let structItemHasGenTypeAnnotation = structItem =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.exists(vb => vb.Typedtree.vb_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | _ => false
  };

let signatureItemHasGenTypeAnnotation = signatureItem =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | _ => false
  };

let cmtHasGenTypeAnnotations = inputCMT =>
  switch (inputCMT.Cmt_format.cmt_annots) {
  | Implementation(structure) =>
    structure.Typedtree.str_items
    |> List.exists(structItemHasGenTypeAnnotation)
  | Interface(signature) =>
    signature.Typedtree.sig_items
    |> List.exists(signatureItemHasGenTypeAnnotation)
  | _ => false
  };

let translateStructItem =
    (~language, ~propsTypeGen, ~moduleName, structItem): CodeItem.translation =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(CodeItem.translateTypeDecl)
    |> CodeItem.combineTranslations

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         CodeItem.translateStructValue(~language, ~propsTypeGen, ~moduleName),
       )
    |> CodeItem.combineTranslations

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription |> CodeItem.translatePrimitive

  | _ => {CodeItem.dependencies: [], CodeItem.codeItems: []}
  /* TODO: Support mapping of variant type definitions. */
  };

let translateSignatureItem =
    (~language, ~propsTypeGen, ~moduleName, signatureItem)
    : CodeItem.translation =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(CodeItem.translateTypeDecl)
    |> CodeItem.combineTranslations

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription |> CodeItem.translatePrimitive;
    } else {
      valueDescription
      |> CodeItem.translateSignatureValue(
           ~language,
           ~propsTypeGen,
           ~moduleName,
         );
    }

  | _ => {CodeItem.dependencies: [], CodeItem.codeItems: []}
  };

let cmtToCodeItems =
    (
      ~config,
      ~propsTypeGen,
      ~moduleName,
      ~outputFileRelative,
      ~resolver,
      inputCMT,
    )
    : list(CodeItem.t) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let translationUnits =
    switch (cmt_annots) {
    | Implementation(structure) =>
      structure.Typedtree.str_items
      |> List.map(structItem =>
           structItem
           |> translateStructItem(
                ~language=config.language,
                ~propsTypeGen,
                ~moduleName,
              )
         )

    | Interface(signature) =>
      signature.Typedtree.sig_items
      |> List.map(signatureItem =>
           signatureItem
           |> translateSignatureItem(
                ~language=config.language,
                ~propsTypeGen,
                ~moduleName,
              )
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
      ~language,
      ~outputFile,
      ~outputFileRelative,
      ~signFile,
      ~resolver,
      codeItems,
    ) => {
  let codeText =
    codeItems
    |> EmitJs.emitCodeItems(~language, ~outputFileRelative, ~resolver);
  let fileContents =
    signFile(EmitTyp.fileHeader(~language) ++ "\n" ++ codeText);

  GeneratedFiles.writeFileIfRequired(~fileName=outputFile, ~fileContents);
};

let processCmtFile = (~signFile, ~config, cmt) => {
  let cmtFile = cmt |> Paths.getCmtFile;
  if (cmtFile != "") {
    let propsTypeGen = GenIdent.createPropsTypeGen();
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
      );
    if (inputCMT |> cmtHasGenTypeAnnotations) {
      inputCMT
      |> cmtToCodeItems(
           ~config,
           ~propsTypeGen,
           ~moduleName,
           ~outputFileRelative,
           ~resolver,
         )
      |> emitCodeItems(
           ~language=config.language,
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