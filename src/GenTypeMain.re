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
  | WrapVariant(_) => "1med"
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
  CodeItem.getGenTypeKind(attributes) != NoGenType
  || attributes
  |> CodeItem.getAttributePayload(tagIsGenTypeImport) != None;

let rec structureItemHasGenTypeAnnotation =
        (structureItem: Typedtree.structure_item) =>
  switch (structureItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.exists(vb => vb.Typedtree.vb_attributes |> hasGenTypeAnnotation)
  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding |> moduleBindingHasGenTypeAnnotation
  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings |> List.exists(moduleBindingHasGenTypeAnnotation)
  | _ => false
  }
and moduleBindingHasGenTypeAnnotation =
    ({mb_expr, mb_attributes, _}: Typedtree.module_binding) =>
  mb_attributes
  |> hasGenTypeAnnotation
  || (
    switch (mb_expr.mod_desc) {
    | Tmod_structure(structure) => structure |> structureHasGenTypeAnnotation
    | Tmod_ident(_)
    | Tmod_functor(_)
    | Tmod_apply(_)
    | Tmod_constraint(_)
    | Tmod_unpack(_) => false
    }
  )
and structureHasGenTypeAnnotation = (structure: Typedtree.structure) =>
  structure.str_items |> List.exists(structureItemHasGenTypeAnnotation);

let rec moduleTypeHasGenTypeAnnotation =
        ({mty_desc, _}: Typedtree.module_type) =>
  switch (mty_desc) {
  | Tmty_signature(signature) => signature |> signatureHasGenTypeAnnotation
  | Tmty_ident(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_)
  | Tmty_alias(_) => false
  }
and moduleDeclarationHasGenTypeAnnotation =
    (moduleDeclaration: Typedtree.module_declaration) =>
  moduleDeclaration.md_attributes
  |> hasGenTypeAnnotation
  || moduleDeclaration.md_type
  |> moduleTypeHasGenTypeAnnotation
and signatureItemHasGenTypeAnnotation =
    (signatureItem: Typedtree.signature_item) =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.exists(dec => dec.Typedtree.typ_attributes |> hasGenTypeAnnotation)
  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    valueDescription.val_attributes |> hasGenTypeAnnotation
  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    moduleDeclaration |> moduleDeclarationHasGenTypeAnnotation
  | _ => false
  }
and signatureHasGenTypeAnnotation = (signature: Typedtree.signature) =>
  signature.Typedtree.sig_items
  |> List.exists(signatureItemHasGenTypeAnnotation);

let cmtHasGenTypeAnnotations = inputCMT =>
  switch (inputCMT.Cmt_format.cmt_annots) {
  | Implementation(structure) => structure |> structureHasGenTypeAnnotation
  | Interface(signature) => signature |> signatureHasGenTypeAnnotation
  | _ => false
  };

let translateStructItem =
    (~language, ~propsTypeGen, ~moduleName, structItem): CodeItem.translation =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(CodeItem.translateTypeDeclaration(~language))
    |> CodeItem.combineTranslations

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         CodeItem.translateValueBinding(
           ~language,
           ~propsTypeGen,
           ~moduleName,
         ),
       )
    |> CodeItem.combineTranslations

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> CodeItem.translatePrimitive(~language, ~moduleName, ~propsTypeGen)

  | _ => {CodeItem.dependencies: [], CodeItem.codeItems: []}
  /* TODO: Support mapping of variant type definitions. */
  };

let translateStructure = (~config, ~propsTypeGen, ~moduleName, structure) =>
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructItem(
            ~language=config.language,
            ~propsTypeGen,
            ~moduleName,
          )
     );

let translateSignatureItem =
    (~language, ~propsTypeGen, ~moduleName, signatureItem)
    : CodeItem.translation =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} =>
    typeDeclarations
    |> List.map(CodeItem.translateTypeDeclaration(~language))
    |> CodeItem.combineTranslations

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> CodeItem.translatePrimitive(~language, ~moduleName, ~propsTypeGen);
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

let translateSignature = (~config, ~propsTypeGen, ~moduleName, signature) =>
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(
            ~language=config.language,
            ~propsTypeGen,
            ~moduleName,
          )
     );

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
  let typeDeclarations =
    (
      switch (cmt_annots) {
      | Implementation(structure) =>
        structure.Typedtree.str_items
        |> List.map(structItem => structItem |> typeDeclarationsOfStructItem)
      | Interface(signature) =>
        signature.Typedtree.sig_items
        |> List.map(signatureItem =>
             signatureItem |> typeDeclarationsOfSignature
           )
      | _ => []
      }
    )
    |> List.concat;
  typeDeclarations
  |> List.map(typeDeclaration =>
       (typeDeclaration |> CodeItem.translateTypeDeclaration(~language)).
         codeItems
     )
  |> List.concat;
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
      structure |> translateStructure(~config, ~propsTypeGen, ~moduleName)
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
    |> EmitJs.emitCodeItems(
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
        ~excludeFile=fname =>
        fname == "React.re" || fname == "ReasonReact.re"
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