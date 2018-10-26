open GenTypeCommon;

let translateValueBinding =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      valueBinding,
    )
    : Translation.t => {
  if (Debug.translation^) {
    logItem("Translate Value Binding\n");
  };
  let {Typedtree.vb_pat, vb_attributes, vb_expr, _} = valueBinding;
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeExpr = vb_expr.exp_type;
  switch (vb_pat.pat_desc, Annotation.fromAttributes(vb_attributes)) {
  | (Tpat_var(id, _), GenType) when Ident.name(id) == "make" =>
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
  | (Tpat_var(id, _), GenType) =>
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

let rec translateModuleBinding =
        (
          ~config,
          ~outputFileRelative,
          ~resolver,
          ~fileName,
          ~typeEnv,
          ~moduleItemGen,
          {mb_id, mb_expr, _}: Typedtree.module_binding,
        )
        : Translation.t => {
  if (Debug.translation^) {
    logItem("Translate Module Binding\n");
  };
  let name = mb_id |> Ident.name;
  let moduleItem = moduleItemGen |> Runtime.newModuleItem;
  typeEnv |> TypeEnv.updateModuleItem(~moduleItem);
  let typeEnv = typeEnv |> TypeEnv.newModule(~name);

  switch (mb_expr.mod_desc) {
  | Tmod_structure(structure) =>
    structure
    |> translateStructure(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )
    |> Translation.combine

  | Tmod_apply(_) =>
    /* Only look at the resulting type of the module */
    switch (mb_expr.mod_type) {
    | Mty_signature(signature) =>
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
      |> Translation.combine

    | Mty_ident(_) =>
      logNotImplemented("Mty_ident" ++ __LOC__);
      Translation.empty;
    | Mty_functor(_) =>
      logNotImplemented("Mty_functor" ++ __LOC__);
      Translation.empty;
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias" ++ __LOC__);
      Translation.empty;
    }

  | Tmod_unpack(_, moduleType) =>
    switch (moduleType) {
    | Mty_signature(signature) =>
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~typeEnv,
         )
      |> Translation.combine

    | Mty_ident(Pident(id)) =>
      switch (
        typeEnv |> TypeEnv.lookupModuleTypeSignature(~name=id |> Ident.name)
      ) {
      | None => Translation.empty
      | Some(signature) =>
        signature
        |> TranslateSignature.translateSignature(
             ~config,
             ~outputFileRelative,
             ~resolver,
             ~fileName,
             ~typeEnv,
           )
        |> Translation.combine
      }

    | Mty_ident(_) =>
      logNotImplemented("Mty_ident " ++ __LOC__);
      Translation.empty;
    | Mty_functor(_) =>
      logNotImplemented("Mty_functor" ++ __LOC__);
      Translation.empty;
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias" ++ __LOC__);
      Translation.empty;
    }

  | Tmod_ident(_) =>
    logNotImplemented("Tmod_ident" ++ __LOC__);
    Translation.empty;
  | Tmod_functor(_) =>
    logNotImplemented("Tmod_functor" ++ __LOC__);
    Translation.empty;
  | Tmod_constraint(_) =>
    logNotImplemented("Tmod_constraint" ++ __LOC__);
    Translation.empty;
  };
}
and translateStructureItem =
    (
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~fileName,
      ~typeEnv,
      structItem,
    )
    : Translation.t =>
  switch (structItem) {
  | {Typedtree.str_desc: Typedtree.Tstr_type(typeDeclarations), _} => {
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

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings), _} =>
    valueBindings
    |> List.map(
         translateValueBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~moduleItemGen,
           ~fileName,
           ~typeEnv,
         ),
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription), _} =>
    /* external declaration */
    valueDescription
    |> Translation.translatePrimitive(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_module(moduleBinding), _} =>
    moduleBinding
    |> translateModuleBinding(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
         ~moduleItemGen,
       )

  | {Typedtree.str_desc: Tstr_modtype(moduleTypeDeclaration), _} =>
    moduleTypeDeclaration
    |> TranslateSignature.translateModuleTypeDeclaration(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~fileName,
         ~typeEnv,
       )

  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings), _} =>
    moduleBindings
    |> List.map(
         translateModuleBinding(
           ~config,
           ~outputFileRelative,
           ~resolver,
           ~fileName,
           ~typeEnv,
           ~moduleItemGen,
         ),
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_include({incl_type: signature}), _} =>
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes(
         ~config,
         ~outputFileRelative,
         ~resolver,
         ~typeEnv,
       )
    |> Translation.combine

  | {Typedtree.str_desc: Tstr_eval(_), _} =>
    logNotImplemented("Tstr_eval" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_typext(_), _} =>
    logNotImplemented("Tstr_typext" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_exception(_), _} =>
    logNotImplemented("Tstr_exception" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_open(_), _} =>
    logNotImplemented("Tstr_open" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class(_), _} =>
    logNotImplemented("Tstr_class" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_class_type(_), _} =>
    logNotImplemented("Tstr_class_type" ++ __LOC__);
    Translation.empty;
  | {Typedtree.str_desc: Tstr_attribute(_), _} =>
    logNotImplemented("Tstr_attribute" ++ __LOC__);
    Translation.empty;
  }
and translateStructure =
    (~config, ~outputFileRelative, ~resolver, ~fileName, ~typeEnv, structure)
    : list(Translation.t) => {
  if (Debug.translation^) {
    logItem("Translate Structure\n");
  };
  let moduleItemGen = Runtime.moduleItemGen();
  structure.Typedtree.str_items
  |> List.map(structItem =>
       structItem
       |> translateStructureItem(
            ~config,
            ~outputFileRelative,
            ~resolver,
            ~moduleItemGen,
            ~fileName,
            ~typeEnv,
          )
     );
};