/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */

module StringSet = Set.Make(String);

open GenTypeCommon;

let cmtHasGenTypeAnnotations = inputCMT =>
  switch (inputCMT.Cmt_format.cmt_annots) {
  | Implementation(structure) =>
    structure |> Annotation.structureHasGenTypeAnnotation
  | Interface(signature) =>
    signature |> Annotation.signatureHasGenTypeAnnotation
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
  (
    typeDeclarations
    |> Translation.translateTypeDeclarations(~language, ~typeEnv)
  ).
    codeItems;
};

let translateCMT = (~config, ~fileName, inputCMT): Translation.t => {
  let {Cmt_format.cmt_annots, _} = inputCMT;
  let typeEnv = TypeEnv.root();
  let translations =
    switch (cmt_annots) {
    | Implementation(structure) =>
      structure
      |> Translation.translateStructure(~config, ~fileName, ~typeEnv)
    | Interface(signature) =>
      signature
      |> Translation.translateSignature(~config, ~fileName, ~typeEnv)
    | _ => []
    };
  translations |> Translation.combine;
};

let emitTranslation =
    (
      ~config,
      ~outputFile,
      ~outputFileRelative,
      ~signFile,
      ~resolver,
      translation,
    ) => {
  let language = config.language;
  let codeText =
    translation
    |> EmitJs.emitTranslationAsString(
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
    let fileName = cmt |> Paths.getModuleName;
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
      |> translateCMT(~config, ~fileName)
      |> emitTranslation(
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