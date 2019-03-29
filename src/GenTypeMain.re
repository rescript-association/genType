/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

let cmtHasGenTypeAnnotations = inputCMT =>
  switch (inputCMT.Cmt_format.cmt_annots) {
  | Implementation(structure) =>
    structure |> Annotation.structureHasGenTypeAnnotation
  | Interface(signature) =>
    signature |> Annotation.signatureHasGenTypeAnnotation
  | _ => false
  };

let structureItemIsDeclaration = structItem =>
  switch (structItem.Typedtree.str_desc) {
  | Typedtree.Tstr_type(_)
  | Tstr_modtype(_)
  | Tstr_module(_) => true
  | _ => false
  };

let signatureItemIsDeclaration = signatureItem =>
  switch (signatureItem.Typedtree.sig_desc) {
  | Typedtree.Tsig_type(_)
  | Tsig_modtype(_) => true
  | _ => false
  };

let inputCmtTranslateTypeDeclarations =
    (~config, ~outputFileRelative, ~resolver, inputCMT)
    : list(CodeItem.typeDeclaration) => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let typeEnv = TypeEnv.root();
  switch (cmt_annots) {
  | Implementation(structure) =>
    {
      ...structure,
      str_items:
        structure.str_items |> List.filter(structureItemIsDeclaration),
    }
    |> TranslateStructure.translateStructure(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> List.map((x: CodeItem.translation) => x.typeDeclarations)
    |> List.concat

  | Interface(signature) =>
    {
      ...signature,
      sig_items:
        signature.sig_items |> List.filter(signatureItemIsDeclaration),
    }
    |> TranslateSignature.translateSignature(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> List.map(x => x.CodeItem.typeDeclarations)
    |> List.concat

  | Packed(_)
  | Partial_implementation(_)
  | Partial_interface(_) => []
  };
};

let translateCMT =
    (~config, ~outputFileRelative, ~resolver, inputCMT): Translation.t => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let typeEnv = TypeEnv.root();
  let translations =
    switch (cmt_annots) {
    | Implementation(structure) =>
      structure
      |> TranslateStructure.translateStructure(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
    | Interface(signature) =>
      signature
      |> TranslateSignature.translateSignature(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
    | _ => []
    };
  translations
  |> Translation.combine
  |> Translation.addTypeDeclarationsFromModuleEquations(~typeEnv);
};

let emitTranslation =
    (
      ~config,
      ~fileName,
      ~outputFile,
      ~outputFileRelative,
      ~resolver,
      ~signFile,
      translation,
    ) => {
  let codeText =
    translation
    |> EmitJs.emitTranslationAsString(
         ~config,
         ~fileName,
         ~outputFileRelative,
         ~resolver,
         ~inputCmtTranslateTypeDeclarations,
       );
  let fileContents =
    signFile(EmitType.fileHeader(~config) ++ "\n" ++ codeText ++ "\n");

  GeneratedFiles.writeFileIfRequired(~outputFile, ~fileContents);
};

let processCmtFile = (~signFile, ~config, cmt) => {
  let cmtFile = cmt |> Paths.getCmtFile;
  if (cmtFile != "") {
    let inputCMT = Cmt_format.read_cmt(cmtFile);
    let outputFile = cmt |> Paths.getOutputFile(~config);
    let outputFileRelative = cmt |> Paths.getOutputFileRelative(~config);
    let fileName = cmt |> Paths.getModuleName;
    let resolver =
      ModuleResolver.createResolver(
        ~extensions=[".re", EmitType.shimExtension(~config)],
        ~excludeFile=fname =>
        fname == "React.re" || fname == "ReasonReact.re"
      );
    if (inputCMT |> cmtHasGenTypeAnnotations) {
      inputCMT
      |> translateCMT(~config, ~outputFileRelative, ~resolver)
      |> emitTranslation(
           ~config,
           ~fileName,
           ~outputFile,
           ~outputFileRelative,
           ~resolver,
           ~signFile,
         );
    } else {
      outputFile |> GeneratedFiles.logFileAction(NoMatch);
      if (Sys.file_exists(outputFile)) {
        Unix.unlink(outputFile);
      };
    };
  };
};