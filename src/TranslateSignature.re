open GenTypeCommon;

let translateSignatureValue =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      valueDescription: Typedtree.value_description,
    )
    : Translation.t => {
  let {Typedtree.val_id, val_desc, val_attributes, _} = valueDescription;
  let typeExpr = val_desc.ctyp_type;
  switch (val_id, Annotation.fromAttributes(val_attributes)) {
  | (id, GenType) when Ident.name(id) == "make" =>
    id
    |> Ident.name
    |> Translation.translateComponent(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | (id, GenType) =>
    id
    |> Ident.name
    |> Translation.translateValue(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~typeExpr,
       )
  | _ => Translation.empty
  };
};

let rec translateModuleDeclaration =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~fileName,
          ~typeEnv,
          {md_id, md_attributes, md_type, _}: Typedtree.module_declaration,
        ) =>
  switch (md_type.mty_desc) {
  | Tmty_signature(signature) =>
    let name = md_id |> Ident.name;
    let _isAnnotated = md_attributes |> Annotation.hasGenTypeAnnotation;
    signature
    |> translateSignature(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
       )
    |> Translation.combine;
  | Tmty_ident(_) =>
    logNotImplemented("Tmty_ident");
    Translation.empty;
  | Tmty_functor(_) =>
    logNotImplemented("Tmty_functor");
    Translation.empty;
  | Tmty_with(_) =>
    logNotImplemented("Tmty_with");
    Translation.empty;
  | Tmty_typeof(_) =>
    logNotImplemented("Tmty_typeof");
    Translation.empty;
  | Tmty_alias(_) =>
    logNotImplemented("Tmty_alias");
    Translation.empty;
  }
and translateSignatureItem =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      signatureItem,
    )
    : Translation.t =>
  switch (signatureItem) {
  | {Typedtree.sig_desc: Typedtree.Tsig_type(typeDeclarations), _} => {
      importTypes: [],
      codeItems: [],
      typeDeclarations:
        typeDeclarations
        |> TranslateTypeDeclarations.translateTypeDeclarations(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~typeEnv,
           ),
    }

  | {Typedtree.sig_desc: Tsig_value(valueDescription), _} =>
    if (valueDescription.val_prim != []) {
      valueDescription
      |> Translation.translatePrimitive(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
         );
    } else {
      let moduleItem = moduleItemGen |> Runtime.newModuleItem;
      typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
      valueDescription
      |> translateSignatureValue(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
         );
    }

  | {Typedtree.sig_desc: Typedtree.Tsig_module(moduleDeclaration), _} =>
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    moduleDeclaration
    |> translateModuleDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       );

  | {Typedtree.sig_desc: Typedtree.Tsig_typext(_), _} =>
    logNotImplemented("Tsig_typext");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_exception(_), _} =>
    logNotImplemented("Tsig_exception");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_recmodule(_), _} =>
    logNotImplemented("Tsig_recmodule");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_modtype(_), _} =>
    logNotImplemented("Tsig_modtype");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_open(_), _} =>
    logNotImplemented("Tsig_open");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_include(_), _} =>
    logNotImplemented("Tsig_include");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_class(_), _} =>
    logNotImplemented("Tsig_class");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_class_type(_), _} =>
    logNotImplemented("Tsig_class_type");
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_attribute(_), _} =>
    logNotImplemented("Tsig_attribute");
    Translation.empty;
  }
and translateSignature =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, signature)
    : list(Translation.t) => {
  let moduleItemGen = Runtime.moduleItemGen();
  signature.Typedtree.sig_items
  |> List.map(signatureItem =>
       signatureItem
       |> translateSignatureItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~fileName,
            ~typeEnv,
          )
     );
};