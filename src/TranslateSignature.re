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
  if (Debug.translation^) {
    logItem("Translate Signature Value %s\n", val_id |> Ident.name);
  };
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
          {md_id, md_type, _}: Typedtree.module_declaration,
        ) => {
  let name = md_id |> Ident.name;
  if (Debug.translation^) {
    logItem("Translate Module Declaration %s\n", name);
  };
  let typeEnv = typeEnv |> TypeEnv.newModule(~name);

  switch (md_type.mty_desc) {
  | Tmty_signature(signature) =>
    signature
    |> translateSignature(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )
    |> Translation.combine

  | Tmty_ident(Pident(id), _) =>
    switch (
      typeEnv |> TypeEnv.lookupModuleTypeSignature(~name=id |> Ident.name)
    ) {
    | None => Translation.empty
    | Some(signature) =>
      signature
      |> translateSignature(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
         )
      |> Translation.combine
    }

  | Tmty_ident(_) =>
    logNotImplemented("Tmty_ident " ++ __LOC__);
    Translation.empty;
  | Tmty_functor(_) =>
    logNotImplemented("Tmty_functor " ++ __LOC__);
    Translation.empty;
  | Tmty_with(_) =>
    logNotImplemented("Tmty_with " ++ __LOC__);
    Translation.empty;
  | Tmty_typeof(_) =>
    logNotImplemented("Tmty_typeof " ++ __LOC__);
    Translation.empty;
  | Tmty_alias(_) =>
    logNotImplemented("Tmty_alias " ++ __LOC__);
    Translation.empty;
  };
}
and translateModuleTypeDeclaration =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~fileName,
      ~typeEnv,
      moduleTypeDeclaration: Typedtree.module_type_declaration,
    ) => {
  if (Debug.translation^) {
    logItem(
      "Translate Module Type Declaration %s\n",
      moduleTypeDeclaration.mtd_id |> Ident.name,
    );
  };
  switch (moduleTypeDeclaration) {
  | {mtd_type: None, _} => Translation.empty
  | {mtd_id, mtd_type: Some(mtd_type), _} =>
    switch (mtd_type.mty_desc) {
    | Tmty_signature(signature) =>
      let name = mtd_id |> Ident.name;
      let translation =
        signature
        |> translateSignature(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~fileName,
             ~typeEnv=typeEnv |> TypeEnv.newModule(~name),
           )
        |> Translation.combine;
      typeEnv |> TypeEnv.addModuleTypeSignature(~name, ~signature);
      translation;

    | Tmty_ident(_) =>
      logNotImplemented("Tmty_ident " ++ __LOC__);
      Translation.empty;
    | Tmty_functor(_) =>
      logNotImplemented("Tmty_functor " ++ __LOC__);
      Translation.empty;
    | Tmty_with(_) =>
      logNotImplemented("Tmty_with " ++ __LOC__);
      Translation.empty;
    | Tmty_typeof(_) =>
      logNotImplemented("Tmty_typeof " ++ __LOC__);
      Translation.empty;
    | Tmty_alias(_) =>
      logNotImplemented("Tmty_alias " ++ __LOC__);
      Translation.empty;
    }
  };
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
    moduleDeclaration
    |> translateModuleDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )

  | {Typedtree.sig_desc: Typedtree.Tsig_modtype(moduleTypeDeclaration), _} =>
    let moduleItem = moduleItemGen |> Runtime.newModuleItem;
    typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
    moduleTypeDeclaration
    |> translateModuleTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       );

  | {Typedtree.sig_desc: Typedtree.Tsig_typext(_), _} =>
    logNotImplemented("Tsig_typext " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_exception(_), _} =>
    logNotImplemented("Tsig_exception " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_recmodule(_), _} =>
    logNotImplemented("Tsig_recmodule " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_open(_), _} =>
    logNotImplemented("Tsig_open " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_include(_), _} =>
    logNotImplemented("Tsig_include " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_class(_), _} =>
    logNotImplemented("Tsig_class " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_class_type(_), _} =>
    logNotImplemented("Tsig_class_type " ++ __LOC__);
    Translation.empty;
  | {Typedtree.sig_desc: Typedtree.Tsig_attribute(_), _} =>
    logNotImplemented("Tsig_attribute " ++ __LOC__);
    Translation.empty;
  }
and translateSignature =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, signature)
    : list(Translation.t) => {
  if (Debug.translation^) {
    logItem("Translate Signature\n");
  };
  let moduleItemGen = Runtime.moduleItemGen();
  signature.Typedtree.sig_items
  |> List.map(
       translateSignatureItem(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~moduleItemGen,
         ~fileName,
         ~typeEnv,
       ),
     );
};